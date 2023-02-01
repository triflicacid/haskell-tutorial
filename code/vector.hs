data Vector = Vector2 {x :: Int, y :: Int} | Vector3 {x :: Int, y :: Int, z :: Int} deriving (Show)

dot :: Vector -> Vector -> Int
dot (Vector2 x y) (Vector2 x' y') = x * x' + y * y'
dot (Vector3 x y z) (Vector3 x' y' z') = x * x' + y * y' + z * z'
dot a b = error $ "Cannot calculate dot product between vectors of differing dimensions: " ++ show a ++ " and " ++ show b

main :: IO ()
main = do
  let u = Vector2 5 (-2)
      v = Vector2 1 2
      w = Vector3 1 2 (-1)
      x = Vector3 5 0 2
  print $ dot u v
  print $ dot w x
  print $ dot u x