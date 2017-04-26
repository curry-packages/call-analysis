-- Example from Bert/Echahed/Ostvold'93

data Nat = O | S Nat

f O = O
f (S x) = f (f x)

main = f x where x free
--> depth-1 analysis: 5 iterations:  main(*) = O
