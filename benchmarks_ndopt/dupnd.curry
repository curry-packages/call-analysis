-- Examples for duplicating non-deterministic computations:


-- arbitrary number between 1 and n:
ndnum :: Int -> Int
ndnum n = if (n==1) then 1 else (n ? ndnum (n-1))

-- lazy non-deterministic duplication:
addNum2 n = let x = ndnum n in x+x
addNum3 n = let x = ndnum n in x+x+x
addNum4 n = let x = ndnum n in x+x+x+x
addNum5 n = let x = ndnum n in x+x+x+x+x


-- arbitrary tuples between 1 and n:
ndpair :: Int -> (Int,Int)
ndpair n   = if (n==1) then (1,1)   else ((n,n)   ? ndpair (n-1))

ndtriple :: Int -> (Int,Int,Int)
ndtriple n = if (n==1) then (1,1,1) else ((n,n,n) ? ndtriple (n-1))

-- lazy non-deterministic duplication:
addPair :: Int -> Int
addPair   n = x+y   where (x,y) = ndpair n

addTriple :: Int -> Int
addTriple n = x+y+z where (x,y,z) = ndtriple n
