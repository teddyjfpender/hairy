{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Field.QM31 (QM31, mkQM31, unQM31)
import Field.CM31 (CM31, unCM31)
import Field.M31  (M31, modulus, unM31)
import Test.QuickCheck
import Data.Word (Word8, Word32)
import Data.Bits ((.|.), shiftL, shiftR)

-- | Random QM31 from raw Word32s.
instance Arbitrary QM31 where
  arbitrary = mkQM31
    <$> choose (0, modulus - 1)
    <*> choose (0, modulus - 1)
    <*> choose (0, modulus - 1)
    <*> choose (0, modulus - 1)

-- | Little-endian bytes to Word32.
bytesToWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32LE b0 b1 b2 b3 =
  fromIntegral b0
    .|. (fromIntegral b1 `shiftL` 8)
    .|. (fromIntegral b2 `shiftL` 16)
    .|. (fromIntegral b3 `shiftL` 24)

-- | Serialize a list of QM31 into bytes (16 bytes each).
intoSlice :: [QM31] -> [Word8]
intoSlice = concatMap $ \qm ->
  let (cm_part1, cm_part2) = unQM31 qm
      (m31_a0, m31_a1)   = unCM31 cm_part1
      (m31_a2, m31_a3)   = unCM31 cm_part2
      w32_a0 = unM31 m31_a0
      w32_a1 = unM31 m31_a1
      w32_a2 = unM31 m31_a2
      w32_a3 = unM31 m31_a3
  in [ fromIntegral w32_a0, fromIntegral (w32_a0 `shiftR` 8), fromIntegral (w32_a0 `shiftR` 16), fromIntegral (w32_a0 `shiftR` 24)
     , fromIntegral w32_a1, fromIntegral (w32_a1 `shiftR` 8), fromIntegral (w32_a1 `shiftR` 16), fromIntegral (w32_a1 `shiftR` 24)
     , fromIntegral w32_a2, fromIntegral (w32_a2 `shiftR` 8), fromIntegral (w32_a2 `shiftR` 16), fromIntegral (w32_a2 `shiftR` 24)
     , fromIntegral w32_a3, fromIntegral (w32_a3 `shiftR` 8), fromIntegral (w32_a3 `shiftR` 16), fromIntegral (w32_a3 `shiftR` 24)
     ]

------------------------------------------------------------------------
-- 1. Inversion
------------------------------------------------------------------------
prop_inverse :: Property
prop_inverse =
  let qm    = mkQM31 1 2 3 4
      qmInv = recip qm
  in qm * qmInv === fromInteger 1

------------------------------------------------------------------------
-- 2. Basic operations
------------------------------------------------------------------------
prop_ops :: Bool
prop_ops =
  let qm0       = mkQM31 1 2 3 4
      qm1       = mkQM31 4 5 6 7
      m         = fromInteger 8 :: QM31
      qm0_x_qm1 = mkQM31 (modulus - 71) 93 (modulus - 16) 50
  in and [ qm0 + qm1 == mkQM31 5 7 9 11
         , qm1 + m     == qm1 + m
         , qm0 * qm1   == qm0_x_qm1
         , qm1 * m     == qm1 * m
         , negate qm0  == mkQM31 (modulus - 1) (modulus - 2) (modulus - 3) (modulus - 4)
         , qm0 - qm1   == mkQM31 (modulus - 3) (modulus - 3) (modulus - 3) (modulus - 3)
         , qm1 - m     == qm1 - m
         , qm0_x_qm1 / qm1 == mkQM31 1 2 3 4
         , qm1 / m     == qm1 / m
         ]

------------------------------------------------------------------------
-- 3. intoSlice round-trip
------------------------------------------------------------------------
prop_intoSlice :: Property
prop_intoSlice = forAll (vectorOf 100 arbitrary) $ \xs ->
  let bs = intoSlice xs
  in conjoin [ xs !! i
               === mkQM31
                     (bytesToWord32LE (bs !! (16*i + 0)) (bs !! (16*i + 1)) (bs !! (16*i + 2)) (bs !! (16*i + 3)))
                     (bytesToWord32LE (bs !! (16*i + 4)) (bs !! (16*i + 5)) (bs !! (16*i + 6)) (bs !! (16*i + 7)))
                     (bytesToWord32LE (bs !! (16*i + 8)) (bs !! (16*i + 9)) (bs !! (16*i + 10)) (bs !! (16*i + 11)))
                     (bytesToWord32LE (bs !! (16*i + 12)) (bs !! (16*i + 13)) (bs !! (16*i + 14)) (bs !! (16*i + 15)))
             | i <- [0..99]
             ]

------------------------------------------------------------------------
-- Run all the tests
------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Running QM31 QuickCheck suite..."
  quickCheck prop_inverse
  quickCheck prop_ops
  quickCheck prop_intoSlice
