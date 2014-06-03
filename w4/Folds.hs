import Data.List

xor :: [Bool] -> Bool
xor = odd . foldr(\x acc -> if x == True then acc+1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr(\x acc -> [f x] ++ acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base (reverse xs)

-- Sieve of Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map(\x -> 2 * x + 1) included
  where ints = [1..n]
        merged = [(x,y) | x <- ints, y <- ints]
        included = ints \\ (nub $ [t | (x, y) <- merged, let t = (x + y + (2 * x * y)), t <= n ])
