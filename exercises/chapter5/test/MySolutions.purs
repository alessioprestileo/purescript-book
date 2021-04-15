module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n - 1)