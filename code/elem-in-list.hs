-- Create a function `elem` which returns whether a given element is in a list

-- METHOD 1
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs)
  | e == x    = True
  | otherwise = elem e xs

-- METHOD 2
elem :: a -> [a] -> Bool
elem e = or (map (\x -> e == x))