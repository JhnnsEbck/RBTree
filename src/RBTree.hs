module RBTree (RBTree(..), insert, inorder) where

-- Red-Black-Tree Datentyp und Funktionen
data Color = Red | Black deriving (Show, Eq)
data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)
              deriving (Show, Eq)

-- Einfügen von Elementen in den Baum
insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = makeBlack (ins tree)
  where
    ins Empty = Node Red Empty x Empty
    ins (Node color left value right)
      | x < value = balance color (ins left) value right
      | x > value = balance color left value (ins right)
      | otherwise = Node color left value right
    makeBlack (Node _ left value right) = Node Black left value right
    makeBlack Empty = Empty


-- Balancing
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance color left value right = Node color left value right

-- Inorder Traversierung
inorder :: RBTree a -> [a]
inorder Empty = []
inorder (Node _ left value right) = inorder left ++ [value] ++ inorder right
