data Nat = O | S Nat

add O x = x
add (S x) y = S (add x y)

double x = add x x

isOne (S O) = True
isOne (S (S _)) = False

not True  = False
not False = True

main = isOne (double x) where x free
