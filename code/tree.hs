-- Tree datatype
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

-- Make an infinite tree with values (left branches, right branches)
makeInfin :: Tree (Int, Int) -> (Int, Int) -> Tree (Int, Int)
makeInfin Leaf tup = makeInfin (Node Leaf tup Leaf) tup
makeInfin (Node lt x rt) (l, r) = Node (makeInfin lt (l + 1, r)) x (makeInfin rt (l, r + 1))

infinTree :: Tree (Int, Int)
infinTree = makeInfin Leaf (0, 0)

-- Cut a tree -- return subtree of a given height
cut :: Int -> Tree a -> Tree a
cut 0 _ = Leaf
cut _ Leaf = Leaf
cut n (Node l x r) = Node (cut (n - 1) l) x (cut (n - 1) r)

-- Insert an element into an ordered tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) =
  if x <= v
    then Node (insert x l) v r
    else Node l v (insert x r)

ordTree :: Tree Int
ordTree = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node Leaf 5 (Node Leaf 6 Leaf))

-- Pre-order tree traversal: parent, left, right
preOrder :: Tree a -> [a]
preOrder Leaf = []
preOrder (Node l x r) = x : preOrder l ++ preOrder r

-- In-order tree traversal: left, parent, right
inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ x : inOrder r

-- Post-order tree traversal: left, right, parent
postOrder :: Tree a -> [a]
postOrder Leaf = []
postOrder (Node l x r) = postOrder l ++ postOrder r ++ [x]