rev :: [a] -> [a]
rev xs = f xs []
    where
        f :: [a] -> [a] -> [a]
        f [] b = b
        f (a:as) b = f as (a:b)

rev' :: [a] -> [a]
-- rev' = foldr (\a b -> b ++ [a]) []
rev' = foldl (\a b -> b : a) []

main :: IO ()
main = do
    let list = [1 .. 5]
    print list
    print (rev' list)