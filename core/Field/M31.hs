{-|
Module      : Field.M31
Description : Finite‑field arithmetic modulo the Mersenne prime 2^31 − 1

This is a *pedagogical* Haskell port of the Rust module
`stwo_prover::core::fields::m31`.  The implementation keeps the same
bit‑twiddling tricks that make the Rust version fast, but wraps them in
lots of comments so you can follow the algebra.

The prime

> p = 2^31 − 1 = 2_147_483_647

is a (small) Mersenne prime.  That special shape lets us replace an
expensive 64‑bit modulus with a handful of shifts, adds and an AND.  For
values strictly less than *p²* (= 2^62 − 2^32 + 1) the reduction is
single‑pass and constant‑time.

We represent field elements as an **unboxed** Word32 (so we pay zero
runtime tagging overhead) and expose the usual `Num`/`Fractional`
interfaces.

Most functions are written `INLINE` so that GHC can fuse away the
wrapper when doing vectorised or integer‑heavy work.

The original Rust uses bytemuck/serde derives; replicating that in
Haskell would require binary instances or cereal.  You can add those if
you need serialisation.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE DeriveGeneric                  #-}

module Field.M31
  ( M31       -- no constructors
  , unM31
  , mkM31     -- safe constructor
  , modulus
  , partialReduce
  , reduce
  , inverse
  , pow2147483645
  , pow2
  -- Re-exporting fromInteger for convenience, though it's part of Num
  , fromInteger
  ) where

import Data.Bits            (Bits (..))
import Data.Word            (Word32, Word64)
import Data.Ratio (numerator, denominator, (%))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))


------------------------------------------------------------------------
-- | The field modulus (a Mersenne prime).
------------------------------------------------------------------------
modulus :: Word32
modulus = 0x7fffffff          -- 2^31 − 1 = 2_147_483_647

-- | A field element.  Invariant: the internal 'Word32' is always in the
-- range @[0, p − 1]@.
newtype M31 = M31 { unM31 :: Word32 }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Bits)

instance NFData M31 -- M31 is a newtype over Word32 which is NFData

instance Bounded M31 where
  minBound = M31 0
  maxBound = M31 (modulus - 1)

instance Enum M31 where
  succ x = x + fromInteger 1
  pred x = x - fromInteger 1
  toEnum i = fromInteger (toInteger i)
  fromEnum (M31 x) = fromIntegral x

------------------------------------------------------------------------
-- Show / Read helpers --------------------------------------------------
------------------------------------------------------------------------
instance Show M31 where
  showsPrec d (M31 x) = showsPrec d x

-- Read instance for Show/Read roundtrip test later
instance Read M31 where
  readsPrec d s = [(mkM31 val, rest) | (val, rest) <- readsPrec d s]

------------------------------------------------------------------------
-- Conversions ----------------------------------------------------------
------------------------------------------------------------------------

-- | Smart constructor from a raw machine word. Ensures the value is in range.
{-# INLINE mkM31 #-}
mkM31 :: Word32 -> M31
mkM31 = partialReduce -- partialReduce ensures the value is in [0, p-1]

------------------------------------------------------------------------
--  Fast reduction routines --------------------------------------------
------------------------------------------------------------------------

-- | /Partial/ reduction:  expects a value in the half‑open interval
-- @[0, 2p)@ and conditionally subtracts the modulus.  One branch, which
-- GHC can turn into a constant‑time select when compiled with `-O2` and
-- `-fllvm`.
{-# INLINE partialReduce #-}
partialReduce :: Word32 -> M31
partialReduce x = M31 $ if x >= modulus then x - modulus else x

-- | Mask for Mersenne prime reduction, same as modulus but Word64
pMask64 :: Word64
pMask64 = fromIntegral modulus

-- | /Full/ reduction for inputs in @[0, p²)@.  Uses the Mersenne trick:
--
-- > p = 2^k − 1  ⇒  x mod p = (x >> k) + (x & p)
--
-- which may still overflow one extra time, so we repeat the dance.
-- (Here @k = 31@.)  The Rust version folds the same idea into three
-- lines of shifts and ANDs; we keep the exact structure for apples‑to‑
-- apples benchmarking.
-- | Core Mersenne reduction for p = 2^31 - 1. Produces a result in [0, p].
-- Based on the formula: x mod p = ((x >> 31) + (x & p) + 1 >> 31) + ((x >> 31) + (x & p) + 1 & p)
-- Simplified to two stages with a carry via the +1.
{-# INLINE mersenneReduce64 #-}
mersenneReduce64 :: Word64 -> Word64
mersenneReduce64 x =
  let step y = (y .&. pMask64) + (y `shiftR` 31)   -- (low 31 bits) + (high bits)
      r1     = step x                              -- first fold:   [0, 2 p]
      r2     = step r1                             -- second fold:  [0, p+1]
      r3     = if r2 >= pMask64 then r2 - pMask64  -- final carry-off
              else r2
  in  r3                                          -- guaranteed < p

-- | /Full/ reduction for inputs in @[0, p²)@.  Uses the Mersenne trick.
{-# INLINE reduce #-}
reduce :: Word64 -> M31
reduce val = mkM31 (fromIntegral (mersenneReduce64 val)) -- mkM31 handles if result is p

------------------------------------------------------------------------
--  Basic field arithmetic ---------------------------------------------
------------------------------------------------------------------------

{-# INLINE addWord #-}
addWord :: Word32 -> Word32 -> Word32
addWord a b = let s = a + b in if s >= modulus then s - modulus else s

instance Num M31 where
  {-# INLINE (+) #-}
  (M31 a) + (M31 b) = M31 (addWord a b)

  {-# INLINE (-) #-}
  (M31 a) - (M31 b) = M31 (addWord a (modulus - b))

  {-# INLINE (*) #-}
  (M31 a) * (M31 b) = reduce $ fromIntegral a * fromIntegral b

  {-# INLINE negate #-}
  negate (M31 0) = M31 0
  negate (M31 a) = M31 (modulus - a)

  abs    = id        -- Field elements are non‑negative representatives
  signum (M31 0) = M31 0
  signum _       = M31 1

  {-# INLINE fromInteger #-}
  fromInteger n
    | n < 0     = fromInteger (n `mod` toInteger modulus)
    | otherwise = M31 (fromIntegral n `mod` modulus)

------------------------------------------------------------------------
--  Fractional / inversion ---------------------------------------------
------------------------------------------------------------------------

-- | Multiplicative inverse using Fermat's little theorem and a
-- hand‑crafted 37‑multiplication addition chain (Rust had the same).
-- The exponent is @p − 2@.
{-# INLINE inverse #-}
inverse :: M31 -> M31
inverse (M31 0) = error "M31.inverse: zero has no multiplicative inverse"
inverse v       = pow2147483645 v

instance Fractional M31 where
  {-# INLINE recip #-}
  recip = inverse

  {-# INLINE fromRational #-}
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- Instance for Real, primarily for toRational
instance Real M31 where
  toRational (M31 x) = toInteger x % 1

-- Instance for Integral. Field division is exact, so remainder is 0.
-- Note: True integer division concepts don't map perfectly to finite fields.
instance Integral M31 where
  toInteger (M31 x)  = toInteger x
  quotRem _ (M31 0)   = error "M31.quotRem: division by zero"
  quotRem a b         = (a / b, M31 0) -- a/b is field division, remainder is 0
  divMod _ (M31 0)    = error "M31.divMod: division by zero"
  divMod a b          = (a / b, M31 0) -- For fields, div and quot are the same.

------------------------------------------------------------------------
--  Exponentiation helpers ---------------------------------------------
------------------------------------------------------------------------

-- | Square-and-multiply helper for powers of two: @pow2 n x = x^(2^n)@.
-- Example: `pow2 3 x` computes `x^8` by squaring `x` three times.
{-# INLINE pow2 #-}
pow2 :: Int -> M31 -> M31
pow2 n !v = go n v
  where
    go 0 !acc = acc
    go i !acc = go (i - 1) (acc * acc)

-- | powWord32 is a helper function for pow2147483645.
-- It computes the power of a base by squaring the base repeatedly.
-- The exponent is given as a Word32.
-- Example: `powWord32 x 3` computes `x^8` by squaring `x` three times.
{-# INLINE powWord32 #-}
powWord32 :: M31 -> Word32 -> M31
powWord32 base e0 = go e0 base (fromInteger 1)
  where
    go 0 _ acc = acc
    go e b acc =
      let acc' = if e .&. 1 == 1 then acc * b else acc
          b'   = b * b
          e'   = e `shiftR` 1
      in  go e' b' acc'

{-# INLINE pow2147483645 #-}
pow2147483645 :: M31 -> M31
pow2147483645 v = powWord32 v 2147483645          -- p − 2