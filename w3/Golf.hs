import Data.List
import Data.Char

skips :: [a] -> [[a]]
skips l = map filter . map (\(_,n) -> n ) $ indexed 
	where indexed  = l `zip` [1..]
	      filter n = [ fst x | x <- indexed, snd x `mod` n == 0 ]

localMaxima :: [Integer] -> [Integer]
localMaxima l@(x:y:z:_)
            | length l < 3  = []
            | length l == 3 = prefix
            | otherwise     = prefix ++ localMaxima (tail l)
            where prefix = if y > x && y > z then [y] else []

levelToString :: Integer -> [(Integer,Integer)] -> String
levelToString level list = map (\(num, sum) -> if sum >= level then '*' else ' ') list ++ "\n"

makeGraph :: [Integer] -> String
makeGraph l = concat $ map(\(n) -> levelToString n list ) [max,(max-1)..1]
    where list    = foldl (\list x -> addToTally x list ) indexed l 
          max     = foldl (\list (x,y) -> if list < y then y else list ) 0 list
          indexed = [0..9] `zip` (repeat 0)
          addToTally i = map (\(num,sum) -> (num, if num == i then sum + 1 else sum) )

histogram :: [Integer] -> String
histogram l = makeGraph l ++ (replicate 10 '=') ++ "\n" ++ (map(\x -> intToDigit x) [0..9]) ++ "\n"
