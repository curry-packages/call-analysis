data Nat = O | S Nat

add O x = x
add (S x) y = S (add x y)

double x = add x x

map _ [] = []
map f (x:xs) = f x : map f xs

main xs = map (add (S O)) (map double xs)
