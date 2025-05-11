module Common.PreludeExtras where

-- Common utility functions used across the tutorials
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x 