-- Queens with set functions

import Control.Search.SetFunctions

insert x []     = [x]
insert x (y:ys) = x:y:ys ? y:insert x ys

perm []     = []
perm (x:xs) = insert x (perm xs)

list n m = if n==m then [m]
                   else n : list (n+1) m

queens n = safe (perm (list 1 n))

safe p | isEmpty (set1 unsafe p) = p

unsafe xs = capture (membersWithDeltaDemand xs)

capture (i,lenZ,j) | abs (i-j)-1 == lenZ = success

abs i = if i<0 then 0-i else i

memberWithRest :: [a] -> (a,[a])
memberWithRest (x:xs) = (x,xs) ? memberWithRest xs

memberWithIndex :: [a] -> (a,Int)
memberWithIndex xs = memberWithIndex' 0 xs
memberWithIndex' i (x:xs) = (x,i) ? memberWithIndex' (i+1) xs


-- slow due to copied non-determinism
membersWithDeltaWhere :: [Int] -> (Int,Int,Int)
membersWithDeltaWhere l = (x,i,y)
  where (x,xs) = memberWithRest l
        (y,i)  = memberWithIndex xs

-- fast due to strict evaluation by case
membersWithDeltaCase :: [a] -> (a,Int,a)
membersWithDeltaCase l =
  case memberWithRest l of
    (x,xs) -> case memberWithIndex xs of
                (y,i) -> (x,i,y)

-- this version will be fast after demand analysis
-- (since the current analysis is not general enough, we add a struict
-- assertion on the computed values to enforce more demand)
membersWithDeltaDemand l | x+i+y>0 = (x,i,y)
  where (x,xs) = memberWithRest l
        (y,i)  = memberWithIndex xs

{-
What's necessary to transform membersWithDeltaWhere into membersWithDeltaCase?

Analyze "result demand" of each operation:
- if unsafe should compute some hnf, capture has to compute some hnf
- if capture should compute some hnf, membersWithDeltaWhere has to compute
  a partial value (*,*,*) (since capture is strict in this partial value)
- if membersWithDeltaWhere should compute (*,*,*), (y,i) and consequently (x,xs)
  has to be computed --> replace where by case (or "strict where")!

Desired implementation of membersWithDeltaWhere:

membersWithDeltaWhere x1 =
  (\x2 ->
    let x3 = membersWithDelta'_dot___hash_selFP5_hash_x x2
        x4 = membersWithDelta'_dot___hash_selFP6_hash_xs x2
     in (\x5 ->
            let x6 = membersWithDelta'_dot___hash_selFP3_hash_y x5
                x7 = membersWithDelta'_dot___hash_selFP4_hash_i x5
             in (x3,x7,x6))
        $! (memberWithIndex x4))
   $! (memberWithRest x1)

-}
