{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck tests for Field.CM31, mirroring the original Rust tests.
module Main (main) where

import Field.CM31
  ( CM31
  , mkCM31
  , unCM31
  , conjugate
  , norm
  , square
  )
import Field.M31
  ( -- M31       -- Type M31 is not directly used, CM31 uses it internally
    unM31     -- To get Word32 from M31 (used in CM31 tests' Arbitrary instance indirectly, and in intoSlice)
  , modulus   -- Used by Arbitrary CM31
  -- The following were found to be unused by the compiler:
  -- , mkM31
  -- , partialReduce
  -- , reduce
  -- , inverse
  -- , pow2147483645
  )
import Test.QuickCheck
import Data.Word (Word8, Word32)
import Data.Bits (shiftL, shiftR, (.|.))

-- Orphan instance for random CM31 values
instance Arbitrary CM31 where
  arbitrary = mkCM31 <$> choose (0, modulus - 1)
                     <*> choose (0, modulus - 1)

-- | Convert four little-endian bytes into a Word32.
bytesToWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32LE b0 b1 b2 b3 =
  fromIntegral b0
    .|. (fromIntegral b1 `shiftL` 8)
    .|. (fromIntegral b2 `shiftL` 16)
    .|. (fromIntegral b3 `shiftL` 24)

-- | Emulate the Rust `into_slice` for a list of CM31 elements.
intoSlice :: [CM31] -> [Word8]
intoSlice = concatMap $ \cm ->
  let (m31_a, m31_b) = unCM31 cm -- Use unCM31 to get the M31 components
      wa = unM31 m31_a           -- Then unM31 to get the Word32 from each M31
      wb = unM31 m31_b
  in [ fromIntegral wa
     , fromIntegral (wa `shiftR` 8)
     , fromIntegral (wa `shiftR` 16)
     , fromIntegral (wa `shiftR` 24)
     , fromIntegral wb
     , fromIntegral (wb `shiftR` 8)
     , fromIntegral (wb `shiftR` 16)
     , fromIntegral (wb `shiftR` 24)
     ]

------------------------------------------------------------------------
-- 1. Inversion test
------------------------------------------------------------------------
prop_inverse :: Bool
prop_inverse =
  let cm   = mkCM31 1 2
      cmInv = recip cm
  in  cm * cmInv == mkCM31 1 0

------------------------------------------------------------------------
-- 2. Basic ops test
------------------------------------------------------------------------
prop_ops :: Bool
prop_ops =
  let cm0       = mkCM31 1 2
      cm1       = mkCM31 4 5
      m         = mkCM31 8 0
      cmFromM   = m
      cm0_x_cm1 = mkCM31 (modulus - 6) 13
  in and [ cm0 + cm1 == mkCM31 5 7
         , cm1 + cmFromM == cm1 + cmFromM
         , cm0 * cm1 == cm0_x_cm1
         , cm1 * cmFromM == cm1 * cmFromM
         , negate cm0 == mkCM31 (modulus - 1) (modulus - 2)
         , cm0 - cm1 == mkCM31 (modulus - 3) (modulus - 3)
         , cm1 - cmFromM == cm1 - cmFromM
         , cm0_x_cm1 / cm1 == mkCM31 1 2
         , cm1 / cmFromM == cm1 / cmFromM
         ]

------------------------------------------------------------------------
-- 3. Additional property tests
------------------------------------------------------------------------

prop_conjugate_involution :: CM31 -> Bool
prop_conjugate_involution x = conjugate (conjugate x) == x

prop_norm_definition :: CM31 -> Bool
prop_norm_definition x =
  let (a, b) = unCM31 x
  in norm x == a * a + b * b

prop_square_definition :: CM31 -> Bool
prop_square_definition x = square x == x * x

prop_norm_mul_conjugate :: CM31 -> Bool
prop_norm_mul_conjugate x =
  let expected = mkCM31 (unM31 (norm x)) 0
  in x * conjugate x == expected

------------------------------------------------------------------------
-- 3. intoSlice round‑trip
------------------------------------------------------------------------
prop_intoSlice :: Property
prop_intoSlice = forAll (vectorOf 100 arbitrary) $ \xs ->
  let bs = intoSlice xs
  in conjoin [ xs !! i
               == mkCM31
                    (bytesToWord32LE (bs !! (8*i + 0)) (bs !! (8*i + 1)) (bs !! (8*i + 2)) (bs !! (8*i + 3)))
                    (bytesToWord32LE (bs !! (8*i + 4)) (bs !! (8*i + 5)) (bs !! (8*i + 6)) (bs !! (8*i + 7)))
             | i <- [0..99]
             ]

------------------------------------------------------------------------
-- Run them all
------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Running CM31 QuickCheck suite..."
  quickCheck prop_inverse
  quickCheck prop_ops
  quickCheck prop_conjugate_involution
  quickCheck prop_norm_definition
  quickCheck prop_square_definition
  quickCheck prop_norm_mul_conjugate
  quickCheck prop_intoSlice

