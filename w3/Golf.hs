skips :: [a] -> [[a]]
skips l = map filter . map (\(_,n) -> n ) $ indexed 
	where indexed  = l `zip` [1..]
	      filter n = [ fst x | x <- indexed, snd x `mod` n == 0 ]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [a] = []
localMaxima [a,b] = []
localMaxima [a,b,c] = if b > a && b > c then [b] else []
localMaxima (x:y:z:rest) = if y > x && y > z 
			then [y] ++ localMaxima (y:z:rest) 
			else localMaxima (y:z:rest)
