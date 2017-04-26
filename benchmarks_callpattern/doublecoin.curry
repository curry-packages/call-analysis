-- The "classical" double coin example:

data Nat = O | S Nat

add O x = x
add (S x) y = S (add x y)

double x = add x x

coin = O
coin = S O

main = double coin

