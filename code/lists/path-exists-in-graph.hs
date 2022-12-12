-- Function which determines whether a path exists in a directed graph
hasPath :: [(Int,Int)] -> Int -> Int -> Bool

-- METHOD 1
hasPath [] _ _ = False  -- Empty graph
hasPath _ x x  = True   -- Start = End
hasPath g x y = 
  let
    g' = [ (n,m) | (n,m) <- g, n /= x ] -- Remove all edges from `x`, prevent loops
  in
    or [ hasPath g' m y | (n,m) <- g, n == x ] -- Traverse all edges starting from `x`, and check if a path exists from the vertex to the destination

-- METHOD 2
hasPath [] _ _ = False
hasPath ((u,v):g') x y =
  (x == y) ||                   -- If start = end
  (x == u && v == y) ||         -- If direct edge exists
  (x == u && hasPath g' v y) || -- If (x,u) is an edge and exists a path v->y
  (v == y && hasPath g' x u) || -- If (v,y) is an edge and exists a path x->u 
  (hasPath g' x y)              -- If a path exists x->y in the graph without (u,v)