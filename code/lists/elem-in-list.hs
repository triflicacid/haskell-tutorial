-- Create a function `elem` which returns whether a given element is in a list
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs)
  | e == x    = True
  | otherwise = elem e xs