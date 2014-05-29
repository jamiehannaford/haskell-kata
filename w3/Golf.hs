module Golf where

skips :: [a] -> [[a]]
skips l = map skip . map (\(x,y) -> (l,y)) $ idx l
    where idx l = l `zip` [1..]
          skip (l,n) = [ fst x | x <- idx l, (snd x) `mod` n == 0 ]
