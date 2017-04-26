-- Examples for duplicating non-deterministic computations cause
-- by free variables:


-- arbitrary number between 1 and n:
findDuplicate s | x+++x =:= s = x  where x free

-- lazy non-deterministic duplication:
l12 n = let x = findDuplicate n in len (x+++x)
l13 n = let x = findDuplicate n in len (x+++x+++x)
l14 n = let x = findDuplicate n in len (x+++x+++x+++x)
l15 n = let x = findDuplicate n in len (x+++x+++x+++x+++x)

[] +++ ys = ys
(x:xs) +++ ys = x : xs+++ys

len [] = 0
len (_:xs) = 1 + len xs

bList n = if n==0 then [] else True : bList (n-1)

doubleBList n = bList n +++ bList n

{-
KiCS2:
l12 ([1..1000]++[1..1000]):  5.84  opt: 0.21
l13 ([1..1000]++[1..1000]): 18.55  opt: 0.21
l15 ([1..1000]++[1..1000]): 70.98  opt: 0.22
-}
main = l15 (doubleBList 100)
