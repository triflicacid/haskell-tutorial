import Prelude hiding (seq)

-- Naive fibonacci generator using explicit recurrence relations
fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n - 1) + fib1 (n - 2)

-- Single recursive approach
fib2 :: Integer -> Integer
fib2 n = aux (0, 1) (n - 1)
  where
    aux :: (Integer, Integer) -> Integer -> Integer
    aux (a, b) 0 = b
    aux (a, b) n = aux (b, a + b) (n - 1)

-- Infinite fibonacci sequence
seq :: [Integer]
seq = 0 : 1 : zipWith (+) seq (tail seq)