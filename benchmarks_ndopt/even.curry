-- Examples for duplicating non-deterministic computations:


-- arbitrary number between 1 and n:
ndnum n = if (n==1) then 1 else (n ? ndnum (n-1))

even x = x `mod` 2 == 0

f x = if even x then 2*x else 2*x+1

testf n = let x = ndnum n in f x
