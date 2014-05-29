toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0 = [] 
    | i < 10 = [i]
    | otherwise = toDigits (i `quot` 10) ++ [(i `rem` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverse . toDigits $ i

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l
    | length l <= 1 = l
    | otherwise = main
    where lastVal = [last l]
          dblPen  = (last $ init l) * 2
          remainder = init $ init l
          main = (doubleEveryOther remainder) ++ (dblPen : lastVal)

sumDigits :: [Integer] -> Integer
sumDigits x = sum . concat . map toDigits $ x

validate :: Integer -> Bool
validate i = sum `rem` 10 == 0
    where sum  = sumDigits list
          list = doubleEveryOther . toDigits $ i
