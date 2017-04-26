import Constraint

-- Non-deterministic insertion in a list
insert x []     = [x]
insert x (y:ys) = (x:y:ys) ? (y:insert x ys)
--insert x ys     = x : ys
--insert x (y:ys) = y : insert x ys

-- Non-deterministic generation of permutations
perm []     = []
perm (x:xs) = insert x (perm xs)

-- permutation sort
psort xs = checkSorted (perm xs)
checkSorted ys | sorted ys = ys

sorted []       = success
sorted [_]      = success
sorted (x:y:ys) = x<=:y & sorted (y:ys)

{-
KiCS2:
psort [13,12..1]:  2.21  opt:  2.23
psort [14,13..1]:  6.92  opt:  6.72
psort [15,14..1]: 20.65  opt: 20.65
-}
