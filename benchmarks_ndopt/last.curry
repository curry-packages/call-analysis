last xs | ys+++[x]=:=xs = x  where x,ys free

[] +++ ys = ys
(x:xs) +++ ys = x : xs+++ys

last2 xs = let x = last xs in x+x
last6 xs = let x = last xs in x+x+x+x+x+x
