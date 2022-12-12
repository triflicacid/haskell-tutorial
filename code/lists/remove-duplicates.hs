-- Remove all duplicates from a given list
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
  | elem x xs = nub xs
  | otherwise = x : nub xs