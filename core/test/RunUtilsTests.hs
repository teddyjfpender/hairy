module Main (main) where

import Utils
import Field.M31 (M31, mkM31)
import Test.QuickCheck
import Data.List (sort)
import System.Exit (exitFailure)

prop_offsetBitReversedCircleDomainIndex :: Bool
prop_offsetBitReversedCircleDomainIndex =
  let domainLogSize = 3
      evalLogSize   = 6
      initialIndex  = 5
      actual = offsetBitReversedCircleDomainIndex initialIndex domainLogSize evalLogSize (-2)
      expectedPrev = previousBitReversedCircleDomainIndex initialIndex domainLogSize evalLogSize
      expectedPrev2 = previousBitReversedCircleDomainIndex expectedPrev domainLogSize evalLogSize
  in actual == expectedPrev2

prop_previousBitReversedCircleDomainIndex :: Property
prop_previousBitReversedCircleDomainIndex = property $ neighborPairs == expectedNeighborPairs
  where
    logSize = 4
    n = shiftL 1 logSize
    values :: [M31]
    values = [ mkM31 (fromIntegral i) | i <- [0..n-1] ]
    bitRevEval = [ values !! bitReverseIndex i logSize | i <- [0..n-1] ]
    neighborPairs = sort
      [ (bitRevEval !! i,
         bitRevEval !! previousBitReversedCircleDomainIndex i (logSize - 3) logSize)
      | i <- [0..n-1] ]
    expectedNeighborPairs = sort
      [ (mkM31 0, mkM31 4)
      , (mkM31 15, mkM31 11)
      , (mkM31 1, mkM31 5)
      , (mkM31 14, mkM31 10)
      , (mkM31 2, mkM31 6)
      , (mkM31 13, mkM31 9)
      , (mkM31 3, mkM31 7)
      , (mkM31 12, mkM31 8)
      , (mkM31 4, mkM31 0)
      , (mkM31 11, mkM31 15)
      , (mkM31 5, mkM31 1)
      , (mkM31 10, mkM31 14)
      , (mkM31 6, mkM31 2)
      , (mkM31 9, mkM31 13)
      , (mkM31 7, mkM31 3)
      , (mkM31 8, mkM31 12)
      ]

prop_circleDomainAndCosetIndexConversion :: Property
prop_circleDomainAndCosetIndexConversion = property $ all check [0..n-1]
  where
    logSize = 3
    n = shiftL 1 logSize
    check i =
      let cosetIdx = circleDomainIndexToCosetIndex i logSize
          circleIdx = cosetIndexToCircleDomainIndex cosetIdx logSize
      in i == circleIdx

run :: Result -> IO Bool
run r = if isSuccess r then pure True else pure False

main :: IO ()
main = do
  putStrLn "Running Utils tests..."
  ok <- and <$> sequence
        [ run =<< quickCheckResult prop_offsetBitReversedCircleDomainIndex
        , run =<< quickCheckResult prop_previousBitReversedCircleDomainIndex
        , run =<< quickCheckResult prop_circleDomainAndCosetIndexConversion
        ]
  if not ok then exitFailure else return ()
