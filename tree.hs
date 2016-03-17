data Tree a = EmptyTree | Tree a (Tree a) (Tree a) deriving (Show)

insertIntoTree :: (Ord a) => Tree a -> a -> Tree a
insertIntoTree EmptyTree v = Tree v EmptyTree EmptyTree
insertIntoTree (Tree x l r) v
  | x == v = Tree v l r
  | x > v  = Tree x (insertIntoTree l v) r
  | x < v  = Tree x l (insertIntoTree r v)
