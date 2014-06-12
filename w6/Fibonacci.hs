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
