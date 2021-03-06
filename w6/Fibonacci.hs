{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib i
  | i <= 1    = i
  | otherwise = (fib (i-1)) + (fib (i-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f z []     = [z]
scanl' f z (x:xs) = let acc = z `f` x in seq acc ( acc : (scanl' f acc xs) )

fibs2 :: [Integer]
fibs2 = scanl' (\acc x -> fib x) 0 [0..]

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
  show l = show . take 20 $ streamToList l

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)

-- e.g. let str = Cons 1 str in streamMap (+1) str
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f s = listToStream $ map f (streamToList s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

{-- ex 5 --}
nats :: Stream Integer
nats = listToStream $ iterate (+1) 0

findN :: Integer -> Integer
findN 0 = 1
findN n = maximum $ takeWhile (\x -> valExp n x) [1..]
  where valExp n exp = (2*n) `mod` (2^exp) == 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) b = Cons a (interleaveStreams b as)

ruler :: Stream Integer
ruler = streamMap findN (listToStream $ iterate (+1) 1)

ruler' :: Stream Integer
ruler' = foldr1 interleaveStreams (map streamRepeat [1..])

{-- ex 6 --}
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap (0-)
  Cons a b + Cons x y = Cons (a + x) (b + y)
  Cons a1 as * b@(Cons b1 bs) = Cons (a1*b1) (streamMap (*a1) bs + as*b)

instance Fractional (Stream Integer) where
  Cons a0 as / Cons b0 bs = q
    where q = Cons (a0 `div` b0) $ streamMap (`div` b0) $ (as - q * bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
