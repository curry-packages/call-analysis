-- Adding three unknown numbers where the last one is positive:

data Nat = O | S Nat

add O x = x
add (S x) y = S (add x y)

main = add x (add y (S z)) where x,y,z free
