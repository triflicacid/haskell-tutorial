-- Echo back the user's input until "quit" is entered
echo :: IO ()
echo = do
  i <- getLine
  if i /= "quit"
    then do
      putStrLn i
      echo
    else return ()

-- Count from "min" to "max" (incl.)
count :: Int -> Int -> IO ()
count min max = do
  putStrLn $ show min
  if min <= max
    then count (min + 1) max
    else return ()
