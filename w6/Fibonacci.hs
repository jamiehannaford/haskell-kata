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
