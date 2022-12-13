data NatNum = Zero | Succ NatNum -- A natural number is either zero, or the successor of another natural number
-- Then, 4 may be defined abstractly as
four :: NatNum
four = Succ $ Succ $ Succ $ Zero

-- Get Int from NatNum
val :: NatNum -> Int
val Zero = 0
val (Succ n) = 1 + (val n)

-- Construct NatNum from Int
build :: Int -> NatNum
build 0 = Zero
build n = Succ $ build (n-1)

incr :: NatNum -> NatNum -- Function to increment a natural number
incr = Succ

decr :: NatNum -> NatNum -- Function to decrement a natural number
decr Zero = error "Cannot decrement zero"
decr (Succ n) = n

add :: NatNum -> NatNum -> NatNum -- Function to add two naturals
add n Zero = n                  -- n+0 = n
add n (Succ m) = Succ (add m n) -- n+(m+1) = (n+m)+1

sub :: NatNum -> NatNum -> NatNum -- Function to subtract two naturals
sub n Zero = n                    -- n-0 = n
sub (Succ n) (Succ m) = sub n m   -- (n+1)-(m+1) = n-m

mul :: NatNum -> NatNum -> NatNum -- Function to multiply two naturals
mul a Zero = Zero                 -- a*0 = 0
mul Zero b = Zero                 -- 0*b = 0
mul a (Succ(Zero)) = a                       -- a*1 = a
mul a (Succ b) = add a (mul a b)  -- a*b = a + a*(b-1)

divs :: NatNum -> NatNum -> NatNum -- Function to divide two naturals
divs a Zero = error "Can't divide by zero"
divs a (Succ(Zero)) = a           -- a/1 = a
divs Zero b = Zero                -- 0/b = 0
divs a b = div_h a b Zero
  where
    div_h Zero b n = n
    div_h a b n = div_h (sub a b) b (Succ(n))