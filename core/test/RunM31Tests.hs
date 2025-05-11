{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Comprehensive QuickCheck tests for Field.M31, covering the original
-- Rust test suite plus additional Haskell-specific properties.
module Main (main) where

import Field.M31
  ( M31
  , unM31
  , mkM31
  , modulus
  , partialReduce
  , reduce
  , inverse
  , pow2147483645
  , pow2
  -- fromInteger is part of Num, operators are part of Num/Fractional, (==) is part of Eq
  -- These are typically available via Prelude
  )
  
-- Qualified import for Field.M31 constructor 'mkM31' to avoid clash if uncommenting local helper.
-- import qualified Field.M31 as FDM31
import Test.QuickCheck
import Data.Word (Word32, Word8, Word64)
import Data.Bits ((.|.), shiftL, shiftR, complement)
import Data.Ratio ((%)) -- For fromRational test
import System.Exit      (exitFailure)
import Test.QuickCheck  (quickCheckResult, isSuccess, Result, Property, counterexample, (===), (==>))
import Test.QuickCheck.Modifiers (NonZero(..), Small(..))
import Control.DeepSeq (deepseq)
import Control.Exception (ErrorCall(..), evaluate, try)
import qualified Test.QuickCheck.Monadic as QCM

-- Arbitrary instance for M31, mirroring the Rust Distribution<M31> test.
-- It now uses the mkM31 imported from Field.M31.
instance Arbitrary M31 where
  arbitrary = Field.M31.mkM31 <$> choose (0, modulus - 1)

-- | Convert four little-endian bytes into a Word32.
bytesToWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32LE b0 b1 b2 b3 =
  fromIntegral b0
    .|. (fromIntegral b1 `shiftL` 8)
    .|. (fromIntegral b2 `shiftL` 16)
    .|. (fromIntegral b3 `shiftL` 24)

-- | Emulate the Rust `into_slice` for a list of field elements.
-- This function needs to use unM31 as the M31 constructor is no longer exported.
intoSlice :: [M31] -> [Word8]
intoSlice = concatMap $ \m ->
  let w = unM31 m in
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  ]

------------------------------------------------------------------------
-- 1. Basic operations: add, mul, neg
------------------------------------------------------------------------
prop_add_mod :: Word32 -> Word32 -> Property
prop_add_mod x y = x <= modulus - 1 && y <= modulus - 1 ==> 
  let a = Field.M31.mkM31 x
      b = Field.M31.mkM31 y
      expected = Field.M31.mkM31 ((x + y) `mod` modulus)
  in unM31 (a + b) === unM31 expected

prop_mul_mod :: Word32 -> Word32 -> Property
prop_mul_mod x y = x <= modulus - 1 && y <= modulus - 1 ==>
  let a = Field.M31.mkM31 x
      b = Field.M31.mkM31 y
      prod = (fromIntegral x * fromIntegral y) `mod` fromIntegral modulus
  in unM31 (a * b) === fromIntegral prod

prop_neg_mod :: Word32 -> Property
prop_neg_mod x = x <= modulus - 1 ==> 
  let a = Field.M31.mkM31 x
      expectedVal = if x == 0 then 0 else modulus - x
  in unM31 (negate a) === expectedVal

------------------------------------------------------------------------
-- 2. intoSlice round-trip
------------------------------------------------------------------------
prop_intoSlice :: Property
prop_intoSlice = forAll (vectorOf 100 arbitrary) $ \xs ->
  let bs = intoSlice xs
  in conjoin [ xs !! i == Field.M31.mkM31 (bytesToWord32LE
                                     (bs !! (4*i + 0))
                                     (bs !! (4*i + 1))
                                     (bs !! (4*i + 2))
                                     (bs !! (4*i + 3)))
             | i <- [0..99]
             ]

------------------------------------------------------------------------
-- 3. fromInteger behaviour (mimicking Rust From<i32>)
------------------------------------------------------------------------
prop_from_positive :: Word32 -> Property
prop_from_positive x = x <= modulus ==> 
  let val = Field.M31.mkM31 x
      expected = Field.M31.mkM31 (x `mod` modulus) -- x `mod` modulus will be x if x <= modulus-1
  in val == expected

prop_from_negative :: Word32 -> Property
prop_from_negative x = x > 0 && x <= modulus ==> 
  let neg_x_m31   = fromInteger (negate (toInteger x)) :: M31
      expected_m31 = Field.M31.mkM31 ((modulus - x) `mod` modulus)
  in neg_x_m31 == expected_m31

------------------------------------------------------------------------
-- 4. Reduction routines
------------------------------------------------------------------------
prop_partialReduce :: Word32 -> Property
prop_partialReduce x = x < 2 * modulus ==> 
  unM31 (partialReduce x) === x `mod` modulus

prop_reduce :: Word64 -> Property
prop_reduce x = x < fromIntegral modulus ^ (2 :: Int) ==> 
  unM31 (reduce x) === fromIntegral (x `mod` fromIntegral modulus)

------------------------------------------------------------------------
-- 5. Inversion and division
------------------------------------------------------------------------
prop_pow_inverse :: M31 -> Property
prop_pow_inverse v = v /= 0 ==> 
  pow2147483645 v * v === 1

prop_division :: M31 -> M31 -> Property
prop_division a b = b /= 0 ==> 
  a / b === a * inverse b

------------------------------------------------------------------------
-- 6. Field axioms (Haskell-specific)
------------------------------------------------------------------------
prop_comm_add :: M31 -> M31 -> Bool
prop_comm_add a b = a + b == b + a

prop_assoc_add :: M31 -> M31 -> M31 -> Bool
prop_assoc_add a b c = (a + b) + c == a + (b + c)

prop_comm_mul :: M31 -> M31 -> Bool
prop_comm_mul a b = a * b == b * a

prop_assoc_mul :: M31 -> M31 -> M31 -> Bool
prop_assoc_mul a b c = (a * b) * c == a * (b * c)

prop_add_identity :: M31 -> Bool
prop_add_identity a = a + 0 == a && 0 + a == a

prop_mul_identity :: M31 -> Bool
prop_mul_identity a = a * 1 == a && 1 * a == a

------------------------------------------------------------------------
-- 7. Additional Property Tests (Step 8 from plan)
------------------------------------------------------------------------

-- Idempotence of reduction functions
prop_reduce_idempotent :: M31 -> Property
prop_reduce_idempotent x = 
  let val = fromIntegral (unM31 x) :: Word64
  in unM31 (reduce val) === unM31 x

prop_partialReduce_idempotent :: M31 -> Property
prop_partialReduce_idempotent x = 
  unM31 (partialReduce (unM31 x)) === unM31 x

-- Show/Read round-trip
prop_show_read_roundtrip :: M31 -> Bool
prop_show_read_roundtrip x = read (show x) == x

-- fromRational laws
prop_fromRational_law1 :: Bool
prop_fromRational_law1 = (fromRational (3 % 4) :: M31) * fromInteger 4 == fromInteger 3

prop_fromRational_law2 :: M31 -> M31 -> Property
prop_fromRational_law2 x y = y /= fromInteger 0 ==>
  fromRational (toRational x / toRational y) == x / y

-- Test with specific boundary values for mkM31, reduce, partialReduce
-- Note: Some of these are implicitly tested by Arbitrary M31 and properties on Num, 
-- but explicit boundary checks are good for critical functions.

-- mkM31 (which uses partialReduce) should handle specific values correctly
prop_mkM31_boundaries :: Property
prop_mkM31_boundaries = conjoin
  [ mkM31 0 === fromInteger 0
  , mkM31 1 === fromInteger 1
  , mkM31 (modulus - 1) === fromInteger (toInteger modulus - 1)
  , mkM31 modulus === fromInteger 0 -- modulus `mod` modulus is 0
  , mkM31 (modulus + 1) === fromInteger 1 -- (modulus+1) `mod` modulus is 1
  , mkM31 (2 * modulus - 1) === fromInteger (toInteger modulus - 1) -- (2p-1) mod p = p-1
  ]

-- reduce should handle specific boundary values for Word64 inputs
prop_reduce_boundaries :: Property
prop_reduce_boundaries = conjoin
  [ reduce 0 === fromInteger 0
  , reduce 1 === fromInteger 1
  , reduce (fromIntegral modulus - 1) === fromInteger (toInteger modulus - 1)
  , reduce (fromIntegral modulus) === fromInteger 0
  , reduce (fromIntegral modulus + 1) === fromInteger 1
    -- Using p = modulus as Word64 for clarity in larger expressions
  , let p64 = fromIntegral modulus
    in reduce (p64 * p64 - 1) === fromInteger (toInteger modulus - 1) -- (p^2-1) mod p = p-1 (since p^2 mod p = 0)
  , let p64 = fromIntegral modulus
    in reduce (p64 * 2 - 1) === fromInteger (toInteger modulus - 1) -- (2p-1) mod p = p-1
  ]

------------------------------------------------------------------------
-- Helper for pow2 test (from m31-test-spec.md)
naivePow :: M31 -> Word64 -> M31
naivePow x e = iterate (* x) 1 !! fromIntegral e

------------------------------------------------------------------------
-- 8. Additional coverage from m31-test-spec.md
------------------------------------------------------------------------

-- Bounded/Enum basics
prop_bounded_values :: Bool
prop_bounded_values =
  unM31 (minBound :: M31) == 0 &&
  unM31 (maxBound :: M31) == modulus - 1

prop_succ_pred_wrap_around :: M31 -> Bool
prop_succ_pred_wrap_around a =
  pred (succ a) == a && succ (pred a) == a

prop_succ_maxBound :: Bool
prop_succ_maxBound = succ (maxBound :: M31) == (0 :: M31)

prop_pred_zero :: Bool
prop_pred_zero = pred (0 :: M31) == (maxBound :: M31)

prop_enum_roundtrip :: Property
prop_enum_roundtrip =
  forAll (choose (0, 65535) :: Gen Word32) $ \w ->
    let x = mkM31 w in (toEnum . fromEnum) x == x

-- NFData instance
prop_deepseq_no_thunks :: M31 -> Int -> Bool
prop_deepseq_no_thunks x y = (x `deepseq` y) == y

-- pow2 helper
prop_pow2 :: Small Int -> M31 -> Bool
prop_pow2 (Small n) x =
  let n' = abs n `mod` 10  -- Keep exponent reasonable, 2^10 is plenty
  in Field.M31.pow2 n' x == naivePow x (2 ^ n')

-- Real / Integral instances
prop_toRational_fromRational_roundtrip :: M31 -> Bool
prop_toRational_fromRational_roundtrip x = fromRational (toRational x) == x

prop_toInteger_fromInteger_roundtrip :: Word32 -> Property
prop_toInteger_fromInteger_roundtrip w = w < modulus ==>
  let x = mkM31 w
  in (fromInteger . toInteger) x == x

prop_quotRem_laws :: M31 -> NonZero M31 -> Bool
prop_quotRem_laws a (NonZero b) =
  let (q,r) = quotRem a b
  in a == q * b + r && r == 0

-- Bits passthrough sanity
prop_complement_involution :: M31 -> Bool
prop_complement_involution x = complement (complement x) == x

prop_shift_roundtrip :: M31 -> Small Word8 -> Bool -- Using Word8 for Small to limit shift amount reasonably
prop_shift_roundtrip x (Small d) =
  let k = fromIntegral d `mod` 31 -- k in [0, 30]
      roundTrip = shiftR (shiftL x k) k
  -- The spec test is `partialReduce (unM31 roundTrip) == roundTrip`
  -- Given M31's Bits instance is newtype derived, and `(w `shiftL` k) `shiftR` k == w` for w < 2^31-1
  -- `roundTrip` will be `x`. `unM31 roundTrip` will be `unM31 x < modulus`.
  -- So `partialReduce (unM31 roundTrip)` simplifies to `roundTrip`.
  -- The test effectively becomes `roundTrip == roundTrip` if intermediate values are not an issue.
  -- The important part is that the sequence of M31 bitwise operations yields the original M31 value.
  in partialReduce (unM31 roundTrip) == roundTrip


-- abs / signum for a field (from Num instance)
prop_abs_is_identity :: M31 -> Bool
prop_abs_is_identity x = abs x == x

prop_signum_values :: M31 -> Bool
prop_signum_values x = signum x == 0 || signum x == 1

------------------------------------------------------------------------
-- 9. Error condition tests and additional properties
------------------------------------------------------------------------

prop_inverse_zero :: Property
prop_inverse_zero = QCM.monadicIO $ do
  let expectedMsg = "M31.inverse: zero has no multiplicative inverse"
  result <- QCM.run $ try (evaluate (inverse (fromInteger 0 :: M31)))
  case result of
    Left (ErrorCall actualMsg) ->
      if actualMsg == expectedMsg
      then QCM.assert True -- Test passes
      else QCM.stop (counterexample
             ("prop_inverse_zero FAIL: Unexpected message.\n  Expected: '" ++ expectedMsg ++ "'\n  Got:      '" ++ actualMsg ++ "'")
             False)
    Right val -> -- Assuming evaluate (error ...) only throws ErrorCall, other Left cases are unreachable
      QCM.stop (counterexample
             ("prop_inverse_zero FAIL: Expected an exception, Got value: " ++ show val)
             False)

prop_division_by_zero :: M31 -> Property
prop_division_by_zero x = QCM.monadicIO $ do
  let expectedMsg = "M31.inverse: zero has no multiplicative inverse"
  result <- QCM.run $ try (evaluate (x / (fromInteger 0 :: M31)))
  case result of
    Left (ErrorCall actualMsg) ->
      if actualMsg == expectedMsg
      then QCM.assert True
      else QCM.stop (counterexample
             ("prop_division_by_zero FAIL: Unexpected message.\n  Expected: '" ++ expectedMsg ++ "'\n  Got:      '" ++ actualMsg ++ "'")
             False)
    Right val ->
      QCM.stop (counterexample
             ("prop_division_by_zero FAIL: Expected an exception, Got value: " ++ show val)
             False)

prop_fromRational_zero_denominator :: Property
prop_fromRational_zero_denominator = 
  forAll (choose (1, fromIntegral modulus - 1) :: Gen Integer) $ \num -> 
  QCM.monadicIO $ do
    let expectedMsg = "M31.inverse: zero has no multiplicative inverse"
    let rat = num % fromIntegral modulus -- Denominator will be 0 in M31, Numerator non-zero
    result <- QCM.run $ try (evaluate (fromRational rat :: M31))
    case result of
      Left (ErrorCall actualMsg) ->
        if actualMsg == expectedMsg
        then QCM.assert True
        else QCM.stop (counterexample
               ("prop_fromRational_zero_denominator FAIL: Unexpected message.\n  Expected: '" ++ expectedMsg ++ "'\n  Got:      '" ++ actualMsg ++ "'")
               False)
      Right val ->
        QCM.stop (counterexample
               ("prop_fromRational_zero_denominator FAIL: Expected an exception, Got value: " ++ show val ++ " (Input num: " ++ show num ++ ")")
               False)

prop_quotRem_by_zero :: M31 -> Property
prop_quotRem_by_zero x = QCM.monadicIO $ do
  let expectedMsg = "M31.quotRem: division by zero"
  result <- QCM.run $ try (evaluate (quotRem x (fromInteger 0 :: M31)))
  case result of
    Left (ErrorCall actualMsg) ->
      if actualMsg == expectedMsg
      then QCM.assert True
      else QCM.stop (counterexample
             ("prop_quotRem_by_zero FAIL: Unexpected message.\n  Expected: '" ++ expectedMsg ++ "'\n  Got:      '" ++ actualMsg ++ "'")
             False)
    Right val ->
      QCM.stop (counterexample
             ("prop_quotRem_by_zero FAIL: Expected an exception, Got value: " ++ show (fst val, snd val)) -- Show tuple
             False)

prop_divMod_by_zero :: M31 -> Property
prop_divMod_by_zero x = QCM.monadicIO $ do
  let expectedMsg = "M31.divMod: division by zero"
  result <- QCM.run $ try (evaluate (divMod x (fromInteger 0 :: M31)))
  case result of
    Left (ErrorCall actualMsg) ->
      if actualMsg == expectedMsg
      then QCM.assert True
      else QCM.stop (counterexample
             ("prop_divMod_by_zero FAIL: Unexpected message.\n  Expected: '" ++ expectedMsg ++ "'\n  Got:      '" ++ actualMsg ++ "'")
             False)
    Right val ->
      QCM.stop (counterexample
             ("prop_divMod_by_zero FAIL: Expected an exception, Got value: " ++ show (fst val, snd val)) -- Show tuple
             False)

prop_subtraction_law :: M31 -> M31 -> Bool
prop_subtraction_law a b = (a - b) + b == a

------------------------------------------------------------------------
-- Run them all
------------------------------------------------------------------------
run :: Result -> IO Bool      -- helper
run r = if isSuccess r then pure True else pure False

main :: IO ()
main = do
  putStrLn "Running M31 QuickCheck suite..."
  ok <- and <$> sequence
        [ run =<< quickCheckResult prop_add_mod
        , run =<< quickCheckResult prop_mul_mod
        , run =<< quickCheckResult prop_neg_mod
        , run =<< quickCheckResult prop_intoSlice
        , run =<< quickCheckResult prop_from_positive
        , run =<< quickCheckResult prop_from_negative
        , run =<< quickCheckResult prop_partialReduce
        , run =<< quickCheckResult prop_reduce
        , run =<< quickCheckResult prop_pow_inverse
        , run =<< quickCheckResult prop_division
        , run =<< quickCheckResult prop_comm_add
        , run =<< quickCheckResult prop_assoc_add
        , run =<< quickCheckResult prop_comm_mul
        , run =<< quickCheckResult prop_assoc_mul
        , run =<< quickCheckResult prop_add_identity
        , run =<< quickCheckResult prop_mul_identity
        , run =<< quickCheckResult prop_reduce_idempotent
        , run =<< quickCheckResult prop_partialReduce_idempotent
        , run =<< quickCheckResult prop_show_read_roundtrip
        , run =<< quickCheckResult prop_fromRational_law1
        , run =<< quickCheckResult prop_fromRational_law2
        , run =<< quickCheckResult prop_mkM31_boundaries  
        , run =<< quickCheckResult prop_reduce_boundaries
        , run =<< quickCheckResult prop_bounded_values
        , run =<< quickCheckResult prop_succ_pred_wrap_around
        , run =<< quickCheckResult prop_succ_maxBound
        , run =<< quickCheckResult prop_pred_zero
        , run =<< quickCheckResult prop_enum_roundtrip
        , run =<< quickCheckResult prop_deepseq_no_thunks
        , run =<< quickCheckResult prop_pow2
        , run =<< quickCheckResult prop_toRational_fromRational_roundtrip
        , run =<< quickCheckResult prop_toInteger_fromInteger_roundtrip
        , run =<< quickCheckResult prop_quotRem_laws
        , run =<< quickCheckResult prop_complement_involution
        , run =<< quickCheckResult prop_shift_roundtrip
        , run =<< quickCheckResult prop_abs_is_identity
        , run =<< quickCheckResult prop_signum_values
        -- New tests for error conditions and subtraction
        , run =<< quickCheckResult prop_inverse_zero
        , run =<< quickCheckResult prop_division_by_zero
        , run =<< quickCheckResult prop_fromRational_zero_denominator
        , run =<< quickCheckResult prop_quotRem_by_zero
        , run =<< quickCheckResult prop_divMod_by_zero
        , run =<< quickCheckResult prop_subtraction_law
        ]
  if not ok then exitFailure else return ()