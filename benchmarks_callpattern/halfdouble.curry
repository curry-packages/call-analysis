data Nat = O | S Nat

add O x = x
add (S x) y = S (add x y)

double x = add x x

isOne O = False
isOne (S O) = True
isOne (S (S _)) = False

-- half is nondeterministic since we define two possible roundings of odd
-- numbers
half O = O
half (S O) = O
half (S O) = S O
half (S (S x)) = S (half x)

-- with call-time choice, main always evaluates to True for all
-- instantiation of x:
main = isOne (double (half x)) where x free
