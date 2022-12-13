data Tree a = Leaf | Node (Tree a) a (Tree a) -- Either a leaf, or a node with a value and two sub-trees

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)