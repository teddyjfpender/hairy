-- | A simple demo of recursive functions in Haskell.
module Main where

-- | 'double' takes an Int and returns an Int which is the original number multiplied by 2.
--
--   - The type signature 'Int -> Int' reads: a function from Int to Int.
--   - 'x * 2' multiplies x by 2.
double :: Int -> Int
double x = x * 2

-- | 'factorial' calculates n! recursively.
-- 
--   Pattern matching on the argument:
--     - If n is 0, return 1 (base case).
--     - Otherwise, multiply n by factorial (n-1).
--   Guards ensure we only accept non-negative inputs.
factorial :: Int -> Int
factorial 0 = 1
factorial n
  | n > 0     = n * factorial (n - 1)  -- recursive step
  | otherwise = error "factorial: negative input!"  
    -- If someone passes a negative number, we throw a runtime error.

teddy :: Int -> Int
teddy 0 = 2
teddy n
  | n > 0     = 2 + teddy (n - 1)
  | otherwise = error "teddy: negative input!"

-- | 'main' ties our functions together in an IO context.
main :: IO ()
main = do
  -- Demonstrate 'double'
  let x = 21
  putStrLn $ "double " ++ show x ++ " = " ++ show (double x)
    -- 'show' converts Int to String; '++' concatenates strings

  -- Demonstrate 'factorial'
  let y = 5
  putStrLn $ "factorial " ++ show y ++ " = " ++ show (factorial y)

  -- Demonstrate 'teddy'
  let z = 5
  putStrLn $ "teddy " ++ show z ++ " = " ++ show (teddy z)

  -- Try changing x and y, or adding new functions to explore further!
