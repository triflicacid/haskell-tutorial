-- Remove if list is in ascending order
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:xs)
  | x <= head xs  = isAsc xs
  | otherwise     = False

main = print (isAsc [1,2,3], isAsc [1,3,2]) -- (True,False)