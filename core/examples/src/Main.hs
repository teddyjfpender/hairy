-- | A simple demo of using the M31 type in Haskell.
module Main where

import Field.M31 -- Import the M31 module to use its types and functions

-- Top-level bindings for signatures
a :: M31
a = fromInteger 10

b :: M31
b = fromInteger 20

c :: M31
c = fromInteger 100

d :: M31
d = fromInteger 200

bigVal1 :: M31
bigVal1 = fromInteger 100000

bigVal2 :: M31
bigVal2 = fromInteger 50000

num :: M31
num = fromInteger 1000

den :: M31
den = fromInteger 50

num2 :: M31
num2 = fromInteger 7

den2 :: M31
den2 = fromInteger 2 

baseForPow :: M31
baseForPow = fromInteger 3

valToInvert :: M31
valToInvert = fromInteger 12345

valFromInt :: M31
valFromInt = fromInteger (modulus_val + 5) where modulus_val = fromIntegral modulus

negValFromInt :: M31
negValFromInt = fromInteger (-5)

-- Helper function to print a description and the result of an M31 computation.
-- This function takes a String (the description) and an M31 value,
-- then performs an IO action (printing to the console).
-- How to read this type signature:
--   "String ->" means it takes a String as its first argument.
--   "M31 ->" means it takes an M31 as its second argument.
--   "IO () " means it performs an Input/Output action and doesn't return a meaningful value (indicated by '()', pronounced "unit").
printM31Example :: String -> M31 -> IO ()
printM31Example description value = putStrLn $ description ++ show value

main :: IO ()
main = do
  -- Haskell's 'do' notation is used to sequence actions, especially IO actions.
  -- Each line in a 'do' block is typically an action.

  putStrLn "--- Demonstrating M31 Arithmetic ---"
  putStrLn $ "The modulus p for M31 is: 2^31 - 1 = " ++ show modulus
  putStrLn "M31 represents numbers in a finite field modulo p."
  putStrLn "All operations (+, -, *) result in values within [0, p-1]."
  putStrLn "" -- Empty line for spacing

  -- Example 1: Basic Addition
  -- 'let' is used to define local variables.
  -- 'a' and 'b' are given M31 types by converting integers using 'fromInteger'.
  -- 'fromInteger' is a function from the 'Num' typeclass that M31 implements.
  -- It intelligently handles conversion to the M31 field, including negative numbers.
  let a = fromInteger 10
  let b = fromInteger 20
  -- The '+' operator for M31 is defined in its 'Num' instance.
  -- It performs addition modulo p.
  printM31Example "a = 10; b = 20. a + b = " (a + b) -- Expected: 30

  -- Example 2: Addition that wraps around the modulus
  let largeA = fromInteger (2^31 - 5) -- This is modulus - 4
  let largeB = fromInteger 10
  -- (modulus - 4) + 10 = modulus + 6. Modulo p, this is 6.
  printM31Example "(2^31 - 5) + 10 = " (largeA + largeB)

  -- Example 3: Subtraction
  -- The '-' operator also works modulo p.
  printM31Example "b - a = " (b - a) -- Expected: 10
  -- Example 3b: Subtraction resulting in a "negative" number (which wraps around)
  -- 10 - 20 = -10. In M31, -10 is equivalent to modulus - 10.
  -- modulus - 10 = (2^31 - 1) - 10 = 2147483647 - 10 = 2147483637
  printM31Example "a - b (10 - 20) = " (a - b)

  -- Example 4: Multiplication
  let c = fromInteger 100
  let d = fromInteger 200
  -- '*' performs multiplication modulo p.
  printM31Example "c = 100; d = 200. c * d = " (c * d) -- Expected: 20000

  -- Example 5: Multiplication resulting in a large number that needs reduction
  let bigVal1 = fromInteger 100000 -- 10^5
  let bigVal2 = fromInteger 50000  -- 5 * 10^4
  -- 100000 * 50000 = 5 * 10^9. This is larger than modulus (approx 2.147 * 10^9).
  -- The result will be (5 * 10^9) mod (2^31 - 1).
  -- 5_000_000_000 mod 2_147_483_647 = 5_000_000_000 - 2 * 2_147_483_647
  -- = 5_000_000_000 - 4_294_967_294 = 705_032_706
  printM31Example "100000 * 50000 = " (bigVal1 * bigVal2)

  -- Example 6: Negation
  -- 'negate' gives the additive inverse. negate x is a value such that x + negate x = 0.
  -- For M31, negate x is (modulus - x) if x is not 0.
  printM31Example "negate a (negate 10) = " (negate a) -- Expected: modulus - 10 = 2147483637
  printM31Example "a + (negate a) = " (a + negate a) -- Expected: 0

  -- Example 7: Conversion from Integer
  -- 'fromInteger' can take any Integer and correctly map it to an M31 element.
  let valFromInt = fromInteger (modulus_val + 5) where modulus_val = fromIntegral modulus
  printM31Example "fromInteger (modulus + 5) = " valFromInt -- Expected: 5
  let negValFromInt = fromInteger (-5)
  -- -5 mod p = p - 5
  printM31Example "fromInteger (-5) = " negValFromInt -- Expected: modulus - 5 = 2147483642

  putStrLn ""
  putStrLn "--- Demonstrating Inverse and Division ---"
  -- M31 implements 'Fractional', so we can use '/' for division.
  -- Division a / b is equivalent to a * (inverse b).
  -- The 'inverse' function finds the multiplicative inverse using Fermat's Little Theorem.
  -- inverse x is a value y such that x * y = 1 (if x is not 0).

  let valToInvert = fromInteger 12345
  -- We can only find an inverse if the number is not zero.
  -- The M31.inverse function will throw an error if you try to invert 0.
  if valToInvert == 0
    then putStrLn "Cannot compute inverse of 0."
    else do
      let invVal = inverse valToInvert
      printM31Example ("inverse " ++ show valToInvert ++ " = ") invVal
      -- Check: valToInvert * inverse valToInvert should be 1
      printM31Example (show valToInvert ++ " * inverse " ++ show valToInvert ++ " = ") (valToInvert * invVal)

  -- Example 8: Division
  let num = fromInteger 1000
  let den = fromInteger 50
  -- 1000 / 50 = 20
  printM31Example "1000 / 50 = " (num / den)

  let num2 = fromInteger 7
  let den2 = fromInteger 2
  -- 7 / 2. This means 7 * (inverse 2).
  -- inverse 2: We need x such that 2x = 1 (mod 2^31 - 1).
  -- Since 2^31 - 1 is odd, (2^31 - 1) + 1 = 2^31 is even.
  -- So, inverse 2 is (modulus + 1) / 2 = (2^31 - 1 + 1) / 2 = 2^31 / 2 = 2^30.
  -- 2^30 = 1073741824.
  -- 7 * 2^30 (mod 2^31 - 1)
  printM31Example "7 / 2 = " (num2 / den2)
  -- Let's verify: 2 * ( (modulus + 1) / 2 ) = modulus + 1 = 1 (mod modulus)
  printM31Example ("Verification: 2 * (inverse 2) = 2 * " ++ show (inverse 2) ++ " = ") (2 * inverse (fromInteger 2))


  putStrLn ""
  putStrLn "--- Demonstrating Exponentiation (pow2147483645) ---"
  -- The M31 module provides a specific exponentiation function:
  -- pow2147483645 calculates x^(p-2), which is used for calculating the inverse (by Fermat's Little Theorem, a^(p-2) = a^(-1) mod p).
  -- Let's test this for a known value.
  let baseForPow = fromInteger 3
  -- inverse 3 should be equal to 3^(p-2)
  if baseForPow == 0
    then putStrLn "Cannot compute inverse of 0 for pow test."
    else do
      let invPow = pow2147483645 baseForPow
      let invDirect = inverse baseForPow
      printM31Example ("pow2147483645 of " ++ show baseForPow ++ " (i.e., " ++ show baseForPow ++ "^(p-2)) = ") invPow
      printM31Example ("inverse of " ++ show baseForPow ++ " (direct calculation) = ") invDirect
      putStrLn $ "Are they equal? " ++ show (invPow == invDirect)
      -- Check: baseForPow * invPow should be 1
      printM31Example (show baseForPow ++ " * (" ++ show baseForPow ++ "^(p-2)) = ") (baseForPow * invPow)

  putStrLn ""
  putStrLn "--- Using M31 in simple expressions ---"
  -- You can combine these operations naturally.
  let x = 5
  let y = 10
  let z = 2
  let expression = (x * y - z) / (x + z) -- (5*10 - 2) / (5+2) = (50 - 2) / 7 = 48 / 7
  -- 48 * inv(7)
  -- inv(7): 7x = 1 mod (2^31-1). (2^31-1) = 306783378 * 7. So 7x = 1 + k*(2^31-1)
  -- inv(7) is pow2147483645 7.
  -- Or, (2^31-1) = 2147483647. 2147483647 = 306783378 * 7 + 1.
  -- So, 1 = 2147483647 - 306783378 * 7
  -- 1 = -306783378 * 7 (mod 2147483647)
  -- inv(7) = -306783378 = 2147483647 - 306783378 = 1840700269
  -- 48 * 1840700269 = 88353612912.
  -- 88353612912 mod 2147483647:
  -- 88353612912 = 41 * 2147483647 + 306783375
  -- So, result is 306783375
  printM31Example "(5*10 - 2) / (5+2) = " expression

  putStrLn ""
  putStrLn "Finished M31 demonstrations." 