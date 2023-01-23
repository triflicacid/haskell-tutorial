lagrange :: [(Float, Float)] -> Float -> Float
lagrange ps x = foldr ((+) . (\(xj, yj) -> yj * l xj)) 0 ps
    where
        l xj = foldr ((*) . (\(xm, ym) -> (x - xm) / (xj - xm))) 1 (filter (\(a, b) -> a /= xj) ps)

main :: IO ()
main = do
    let
        list = [(x,x^2) | x <- [1..3]] :: [(Float, Float)]
        f = lagrange list
    print list
    print (f 5)