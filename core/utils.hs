module Utils where

import Data.Bits
import Data.Word
import Data.Array (Array, array, elems)

import Field.M31 (M31)

-- | Assign elements from second list to first list positionally.
assign :: [a] -> [a] -> [a]
assign xs ys = zipWith (\_ b -> b) xs ys

-- | Take elements from a list while a predicate holds, returning the prefix and the remainder.
peekTakeWhile :: (a -> Bool) -> [a] -> ([a],[a])
peekTakeWhile = span


-- | Returns the bit reversed index of @i@ which is represented by @logSize@ bits.
bitReverseIndex :: Int -> Int -> Int
bitReverseIndex i logSize
  | logSize == 0 = i
  | otherwise    = fromIntegral ((bitReverse w) `shiftR` shift)
  where
    w     = fromIntegral i :: Word
    shift = finiteBitSize w - logSize

-- | Returns the index of the previous element in a bit reversed circle domain
-- relative to a smaller domain of size @domainLogSize@.
previousBitReversedCircleDomainIndex :: Int -> Int -> Int -> Int
previousBitReversedCircleDomainIndex i domainLogSize evalLogSize =
  offsetBitReversedCircleDomainIndex i domainLogSize evalLogSize (-1)

-- | Helper: Euclidean modulus for possibly negative numbers.
modE :: Int -> Int -> Int
modE a m = let r = a `mod` m in if r < 0 then r + m else r

-- | Returns the index of the offset element in a bit reversed circle domain
-- relative to a smaller domain of size @domainLogSize@.
offsetBitReversedCircleDomainIndex :: Int -> Int -> Int -> Int -> Int
offsetBitReversedCircleDomainIndex i domainLogSize evalLogSize offset =
  bitReverseIndex prevIndex' evalLogSize
  where
    halfSize = 1 `shiftL` (evalLogSize - 1)
    stepSize = offset * (1 `shiftL` (evalLogSize - domainLogSize - 1))
    initial  = bitReverseIndex i evalLogSize
    prevIndex
      | initial < halfSize = modE (initial + stepSize) halfSize
      | otherwise          = modE (initial - stepSize) halfSize + halfSize
    prevIndex' = prevIndex

-- | Convert values from circle-domain order to coset order.
circleDomainOrderToCosetOrder :: [M31] -> [M31]
circleDomainOrderToCosetOrder values = concat [ [values !! i, values !! (n - 1 - i)]
                                              | i <- [0 .. n `div` 2 - 1] ]
  where
    n = length values

-- | Convert values from coset order to circle-domain order.
cosetOrderToCircleDomainOrder :: [a] -> [a]
cosetOrderToCircleDomainOrder values = firstHalf ++ secondHalf
  where
    n        = length values
    halfLen  = n `div` 2
    firstHalf  = [ values !! (i `shiftL` 1) | i <- [0 .. halfLen - 1] ]
    secondHalf = [ values !! (n - 1 - (i `shiftL` 1)) | i <- [0 .. halfLen - 1] ]

-- | Convert an index within a circle domain to the corresponding index in a coset.
circleDomainIndexToCosetIndex :: Int -> Int -> Int
circleDomainIndexToCosetIndex circleIndex logDomainSize =
  if circleIndex < n `div` 2
     then circleIndex * 2
     else (n - 1 - circleIndex) * 2 + 1
  where
    n = 1 `shiftL` logDomainSize

-- | Convert an index within a coset to the corresponding index in a circle domain.
cosetIndexToCircleDomainIndex :: Int -> Int -> Int
cosetIndexToCircleDomainIndex cosetIndex logDomainSize =
  if cosetIndex `mod` 2 == 0
     then cosetIndex `div` 2
     else ((2 `shiftL` logDomainSize) - cosetIndex) `div` 2

-- | Perform a coset-natural-order to circle-domain-bit-reversed-order permutation.
bitReverseCosetToCircleDomainOrder :: [a] -> [a]
bitReverseCosetToCircleDomainOrder v
  | n == 0 = []
  | n .&. (n - 1) /= 0 = error "length must be power of two"
  | otherwise = elems arr
  where
    n     = length v
    logN  = ilog2 n
    arr :: Array Int a
    arr   = array (0,n-1) [ (j, v !! i) | i <- [0..n-1]
                         , let j = bitReverseIndex (cosetIndexToCircleDomainIndex i logN) logN ]

ilog2 :: Int -> Int
ilog2 x
  | x <= 0    = error "ilog2: non-positive"
  | otherwise = finiteBitSize (0 :: Word) - 1 - countLeadingZeros (fromIntegral x :: Word)

-- | Unsafe uninitialized vector. Here implemented as a list of undefined values.
uninitVec :: Int -> [a]
uninitVec len = replicate len (error "uninitialized element")

