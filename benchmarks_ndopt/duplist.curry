-- Examples for duplicating non-deterministic computations cause
-- by free variables:


-- arbitrary number between 1 and n:
findDuplicate s | x+++x =:= s = x  where x free

-- lazy non-deterministic duplication:
dupList2 n = let x = findDuplicate n in len (x+++x)
dupList3 n = let x = findDuplicate n in len (x+++x+++x)
dupList4 n = let x = findDuplicate n in len (x+++x+++x+++x)
dupList5 n = let x = findDuplicate n in len (x+++x+++x+++x+++x)

[] +++ ys = ys
(x:xs) +++ ys = x : xs+++ys

len [] = 0
len (_:xs) = 1 + len xs

{-
KiCS2:
dupList2 ([1..1000]++[1..1000]):  5.84  opt: 0.21
dupList3 ([1..1000]++[1..1000]): 18.55  opt: 0.21
dupList5 ([1..1000]++[1..1000]): 70.98  opt: 0.22
-}
