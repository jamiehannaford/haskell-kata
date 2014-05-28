import Data.Char

toDigits :: String -> Int
toDigits []     = 0
toDigits (x:xs) = (digitToInt(x) * 10 ^ length (xs)) + (toDigits xs)
