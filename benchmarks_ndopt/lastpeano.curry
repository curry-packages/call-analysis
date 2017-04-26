-- Computing last element of a list and use it multiple times

data Peano = O | S Peano

toPeano :: Int -> Peano
toPeano n = if n==0 then O else S (toPeano (n-1))

fromPeano :: Peano -> Int
fromPeano O = 0
fromPeano (S x) = fromPeano x + 1

equal :: Peano -> Peano -> Bool
equal O O = True
equal (S p) (S q) = equal p q
equal (S _) O = False
equal O (S _) = False

add :: Peano -> Peano -> Peano
add O     p = p
add (S p) q = S (add p q)

longList n = if n==0 then [] else True : longList (n-1)

last xs | ys+++[x]=:=xs = x  where x,ys free

[] +++ ys = ys
(x:xs) +++ ys = x : xs+++ys

and True  True  = True
and True  False = True
and False True  = True
and False False = False

andLast2 xs = let x = last xs in and x x
andLast4 xs = let x = last xs in (and (and x x) (and x x))
andLast6 xs = let x = last xs in and (and x x) (and (and x x) (and x x))
--addLast3 xs = let x = last xs in add x (add x x)
--addLast4 xs = let x = last xs in add (add x x) (add x x)
main = andLast6 (longList 500)