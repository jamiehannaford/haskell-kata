data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node i _ _ _) = i

insert :: Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf
insert (Node i t1 y t2) x = if h1 < h2
        then Node (h2 + 1) (insert t1 x) y t2
        else Node (h1 + 1) t1 y (insert t2 x)
    where h1 = height t1
          h2 = height t2

foldTree :: [a] -> Tree a
foldTree = foldr(\x acc -> insert acc x) Leaf
