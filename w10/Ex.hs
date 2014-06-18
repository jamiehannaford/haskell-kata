data Tree a = Node a [Tree a]

instance Functor Tree where
  fmap f (Node a ns) = Node (f a) (map (fmap f) ns)

instance Functor Either where
  fmap (Left x)  = Left x
  fmap (Right x) = Right (f x)

instance Functor ((->) t) where
  fmap = (.)
