{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Field.CM31
  ( CM31       -- opaque complex‐field element
  , unCM31
  , mkCM31     -- safe constructor from raw words
  , conjugate  -- explicit conjugation
  , norm       -- explicit norm
  , square     -- explicit squaring
  ) where

import Data.Word            (Word32)
import GHC.Generics         (Generic)
import Control.DeepSeq      (NFData)
import Foreign.Storable     (Storable(..))
import Field.M31            (M31, mkM31)

-- | Complex extension field of M31: a + b i with i^2 = -1.
data CM31 = CM31
  {-# UNPACK #-} !M31   -- ^ real part
  {-# UNPACK #-} !M31   -- ^ imag part
  deriving stock   (Eq, Ord, Generic)
  deriving anyclass (NFData, Storable)

-- | Reveal the two components.
unCM31 :: CM31 -> (M31, M31)
unCM31 (CM31 a b) = (a, b)

-- | Smart constructor from two raw Word32s, reduced mod p.
{-# INLINE mkCM31 #-}
mkCM31 :: Word32 -> Word32 -> CM31
mkCM31 a b = CM31 (mkM31 a) (mkM31 b)

-- | Complex conjugation: a + bi -> a - bi
{-# INLINE conjugate #-}
conjugate :: CM31 -> CM31
conjugate (CM31 a b) = CM31 a (negate b)

-- | Field norm: N(a + bi) = a² + b²
{-# INLINE norm #-}
norm :: CM31 -> M31
norm (CM31 a b) = a * a + b * b

-- | Squaring: (a + bi)² = (a² - b²) + 2abi
{-# INLINE square #-}
square :: CM31 -> CM31
square (CM31 a b) = CM31 (a * a - b * b) (2 * a * b)

instance Show CM31 where
  showsPrec d (CM31 a b) =
    showsPrec d a
    . showString " + "
    . showsPrec d b
    . showString "i"

instance Num CM31 where
  {-# INLINE (+) #-}
  (CM31 a b) + (CM31 c d) = CM31 (a + c) (b + d)

  {-# INLINE (-) #-}
  (CM31 a b) - (CM31 c d) = CM31 (a - c) (b - d)

  {-# INLINE (*) #-}
  (CM31 a b) * (CM31 c d) =
    -- (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
    CM31 (a * c - b * d)
         (a * d + b * c)

  {-# INLINE negate #-}
  negate (CM31 a b) = CM31 (negate a) (negate b)

  abs    = id              -- no canonical ordering beyond Eq/Ord
  signum _ = 1             -- every non-zero is a unit
  {-# INLINE fromInteger #-}
  fromInteger n = CM31 (fromInteger n) 0

instance Fractional CM31 where
  {-# INLINE recip #-}
  recip (CM31 a b) =
    -- 1/(a + bi) = (a - bi)/(a^2 + b^2)
    let denom = norm (CM31 a b)
    in CM31 ( a / denom) (negate b / denom)

  {-# INLINE fromRational #-}
  fromRational r = CM31 (fromRational r) 0

-- | RULES for fusing conjugate . conjugate into id
{-# RULES
"conjugate/conjugate" forall x. conjugate (conjugate x) = x
  #-}
