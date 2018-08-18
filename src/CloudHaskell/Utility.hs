module CloudHaskell.Utility where

toMicroseconds :: Int -> Int
toMicroseconds i = i * 1000000

toMilliseconds :: Int -> Int
toMilliseconds i = i * 1000

calculateAnswer :: [Double] -> ([Double], Double)
calculateAnswer msgs = (msgs, sum $ zipWith (*) [1..] msgs)
