-- Examples for duplicating non-deterministic computations caused
-- by free variables:

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

half y | equal (add x x) (toPeano y) = fromPeano x where x free

-- lazy non-deterministic duplication:
half2 n = let x = half n in x+x
half3 n = let x = half n in x+x+x
half4 n = let x = half n in x+x+x+x
half5 n = let x = half n in x+x+x+x+x

{-
KiCS2:
half2 2000 -> 4.83  opt: 2.10
half5 2000 -> 9.19  opt: 2.10
-}
