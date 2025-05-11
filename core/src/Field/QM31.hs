{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Field.QM31
  ( QM31
  , unQM31
  , mkQM31
  , fromPartialEvals
  , mulCM31
  ) where

import Data.Word       (Word32)
import Data.Ratio      (numerator, denominator)
import GHC.Generics    (Generic)
import Control.DeepSeq (NFData)
import Field.CM31      (CM31, mkCM31, unCM31)
import Field.M31       (M31, unM31)

-- | Quartic extension of M31: (a + b u) with u^2 = 2 + i.
--   Represented as QM31 a b.
data QM31 = QM31
  {-# UNPACK #-} !CM31  -- ^ coefficient of 1
  {-# UNPACK #-} !CM31  -- ^ coefficient of u
  deriving stock   (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Split into the two CM31 components.
unQM31 :: QM31 -> (CM31, CM31)
unQM31 (QM31 a b) = (a, b)

-- | Smart constructor from four raw Word32s.
{-# INLINE mkQM31 #-}
mkQM31 :: Word32 -> Word32 -> Word32 -> Word32 -> QM31
mkQM31 a0 a1 a2 a3 =
  QM31 (mkCM31 a0 a1) (mkCM31 a2 a3)

-- | Constant R = 2 + i in CM31.
r :: CM31
r = mkCM31 2 1

-- | Multiply a QM31 by a CM31 scalar.
{-# INLINE mulCM31 #-}
mulCM31 :: QM31 -> CM31 -> QM31
mulCM31 (QM31 a b) c = QM31 (a * c) (b * c)

-- | Combine four partial evaluations into one QM31:
--   e0 + e1*u + e2*u^2 + e3*u^3
fromPartialEvals :: [QM31] -> QM31
fromPartialEvals [e0, e1, e2, e3] =
  e0
  + (e1 * mkQM31 0 0 1 0)  -- u
  + (e2 * mkQM31 2 1 0 0)  -- u^2 = r = 2+i
  + (e3 * mkQM31 0 0 2 1)  -- u^3 = r*u = (2+i)u
fromPartialEvals _ = error "Field.QM31.fromPartialEvals: need exactly 4 elements"

instance Show QM31 where
  showsPrec d (QM31 a b) =
    showsPrec d a
    . showString " + "
    . showsPrec d b
    . showString "u"

instance Num QM31 where
  {-# INLINE (+) #-}
  (QM31 a b) + (QM31 c d) = QM31 (a + c) (b + d)
  {-# INLINE (-) #-}
  (QM31 a b) - (QM31 c d) = QM31 (a - c) (b - d)
  {-# INLINE (*) #-}
  (QM31 a b) * (QM31 c d) =
    -- (a + b u) * (c + d u) = (a c + R * b d) + (a d + b c) u
    QM31 (a * c + r * (b * d))
         (a * d + b * c)
  {-# INLINE negate #-}
  negate (QM31 a b) = QM31 (negate a) (negate b)
  abs    = id
  signum _ = 1
  {-# INLINE fromInteger #-}
  fromInteger n = QM31 (fromInteger n) 0

instance Fractional QM31 where
  {-# INLINE recip #-}
  recip (QM31 a b) =
    let b2          = b * b                      -- CM31
        (b2_real, b2_imag) = unCM31 b2          -- destructure CM31 using unCM31
        -- ib2 corresponds to i * b^2. If b^2 = x + yi, then i*b^2 = -y + xi.
        -- mkCM31 takes (real, imag) parts as Word32.
        ib2         = mkCM31 (unM31 (negate b2_imag)) (unM31 b2_real) -- construct CM31 using mkCM31
        denom       = a * a - (b2 + b2 + ib2)    -- CM31
        denomInv    = recip denom                 -- CM31 inverse
    in QM31 (a * denomInv) (negate b * denomInv)
  {-# INLINE fromRational #-}
  fromRational r' = fromInteger (numerator r') / fromInteger (denominator r')
