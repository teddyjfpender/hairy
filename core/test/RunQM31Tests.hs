{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import qualified Field.QM31 as Q
  ( QM31
  , mkQM31
  , unQM31
  , fromPartialEvals
  , mulCM31
  , conjugate
  , norm
  , square
  )
import qualified Field.CM31 as C
  ( CM31
  , mkCM31
  , unCM31
  , conjugate
  , norm
  , square
  )
import Field.M31  (M31, modulus, unM31)
import Test.QuickCheck
import Data.Word (Word8, Word32)
import Data.Bits ((.|.), shiftL, shiftR)
import Control.Exception (ErrorCall(..), evaluate, try)
import qualified Test.QuickCheck.Monadic as QCM

-- | Random QM31 from raw Word32s.
instance Arbitrary Q.QM31 where
  arbitrary = Q.mkQM31
    <$> choose (0, modulus - 1)
    <*> choose (0, modulus - 1)
    <*> choose (0, modulus - 1)
    <*> choose (0, modulus - 1)

instance Arbitrary C.CM31 where
  arbitrary = C.mkCM31 <$> choose (0, modulus - 1)
                       <*> choose (0, modulus - 1)

-- | Little-endian bytes to Word32.
bytesToWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32LE b0 b1 b2 b3 =
  fromIntegral b0
    .|. (fromIntegral b1 `shiftL` 8)
    .|. (fromIntegral b2 `shiftL` 16)
    .|. (fromIntegral b3 `shiftL` 24)

-- | Serialize a list of QM31 into bytes (16 bytes each).
intoSlice :: [Q.QM31] -> [Word8]
intoSlice = concatMap $ \qm ->
  let (cm_part1, cm_part2) = Q.unQM31 qm
      (m31_a0, m31_a1)   = C.unCM31 cm_part1
      (m31_a2, m31_a3)   = C.unCM31 cm_part2
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
  let qm    = Q.mkQM31 1 2 3 4
      qmInv = recip qm
  in qm * qmInv === fromInteger 1

------------------------------------------------------------------------
-- 2. Basic operations
------------------------------------------------------------------------
prop_ops :: Bool
prop_ops =
  let qm0       = Q.mkQM31 1 2 3 4
      qm1       = Q.mkQM31 4 5 6 7
      m         = fromInteger 8 :: Q.QM31
      qm0_x_qm1 = Q.mkQM31 (modulus - 71) 93 (modulus - 16) 50
  in and [ qm0 + qm1 == Q.mkQM31 5 7 9 11
         , qm1 + m     == qm1 + m
         , qm0 * qm1   == qm0_x_qm1
         , qm1 * m     == qm1 * m
         , negate qm0  == Q.mkQM31 (modulus - 1) (modulus - 2) (modulus - 3) (modulus - 4)
         , qm0 - qm1   == Q.mkQM31 (modulus - 3) (modulus - 3) (modulus - 3) (modulus - 3)
         , qm1 - m     == qm1 - m
         , qm0_x_qm1 / qm1 == Q.mkQM31 1 2 3 4
         , qm1 / m     == qm1 / m
         ]

------------------------------------------------------------------------
-- 3. Additional property tests
------------------------------------------------------------------------

prop_conjugate_involution :: Q.QM31 -> Bool
prop_conjugate_involution x = Q.conjugate (Q.conjugate x) == x

prop_norm_definition :: Q.QM31 -> Bool
prop_norm_definition x =
  let (realPart, imagPart) = Q.unQM31 (x * Q.conjugate x)
  in imagPart == C.mkCM31 0 0 && realPart == Q.norm x

prop_square_definition :: Q.QM31 -> Bool
prop_square_definition x = Q.square x == x * x

prop_mulCM31_definition :: Q.QM31 -> C.CM31 -> Bool
prop_mulCM31_definition x c =
  let (a,b) = Q.unQM31 x
      (rA,rB) = Q.unQM31 (Q.mulCM31 x c)
  in rA == a * c && rB == b * c

prop_fromPartialEvals_definition :: Q.QM31 -> Q.QM31 -> Q.QM31 -> Q.QM31 -> Bool
prop_fromPartialEvals_definition e0 e1 e2 e3 =
  Q.fromPartialEvals [e0,e1,e2,e3]
    == e0 + e1 * Q.mkQM31 0 0 1 0
         + e2 * Q.mkQM31 2 1 0 0
         + e3 * Q.mkQM31 0 0 2 1

prop_fromPartialEvals_error :: [Q.QM31] -> Property
prop_fromPartialEvals_error xs =
  length xs /= 4 ==> QCM.monadicIO $ do
    result <- QCM.run $ try (evaluate (Q.fromPartialEvals xs))
    case result of
      Left (ErrorCall msg) ->
        QCM.assert (msg == "Field.QM31.fromPartialEvals: need exactly 4 elements")
      Right _ -> QCM.stop (counterexample "expected exception" False)

------------------------------------------------------------------------
-- 3. intoSlice round-trip
------------------------------------------------------------------------
prop_intoSlice :: Property
prop_intoSlice = forAll (vectorOf 100 arbitrary) $ \xs ->
  let bs = intoSlice xs
  in conjoin [ xs !! i
               === Q.mkQM31
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
  quickCheck prop_conjugate_involution
  quickCheck prop_norm_definition
  quickCheck prop_square_definition
  quickCheck prop_mulCM31_definition
  quickCheck prop_fromPartialEvals_definition
  quickCheck prop_fromPartialEvals_error
  quickCheck prop_intoSlice
