prefixes :: [a] -> [[a]]
prefixes xs = tail $ foldl (\acc x -> acc ++ [last acc ++ [x]]) [[]] xs

main :: IO ()
main = do
    let list = [1,2,3]
    print list
    print (prefixes list)