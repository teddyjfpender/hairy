-- | This is the entry point of our "Hello, Haskell!" program.
module Main where

-- | The type signature of 'main'. 
--   'IO ()' means an IO action that returns no meaningful value.
main :: IO ()
main = do
  -- putStrLn prints a string followed by a newline to stdout
  putStrLn "Hello, Haskell!"  

  -- You can chain more IO actions here. For example:
  -- putStrLn "This is still Haskell!"
  putStrLn "This is still Haskell!!"
