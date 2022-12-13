-- An expression may either be a number, or an operator of two more expressions
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval :: Expr -> Int -- Function to evaluate an expression
eval (Val a) = a
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Div x y) = (eval x) `div` (eval y)

main = print (eval ((Val 5) `Add` (Val 3))) -- Output: 8