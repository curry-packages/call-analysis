-- Computing last element of a list and use it multiple times

trueList n = if n==0 then [] else True : trueList (n-1)

last xs | ys++[x]=:=xs = x  where x,ys free

andLast xs = let x = last xs in x && x && x && x && x

main = andLast (trueList 1000)