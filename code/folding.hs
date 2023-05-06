foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ b [] = b
foldRight f b (x : xs) = f x (foldRight f b xs)

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ b [] = b
foldLeft f b (x : xs) = f (foldLeft f b xs) x