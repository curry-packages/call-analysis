{-
-- fast
select (x:xs) = (x, xs) ? (let (y, ys) = select xs in (y, x:ys))

-- slow
select2 xs = (m, rest)
  where
    m = someOf xs
    rest = del m xs

-}

someOf (x:xs) = x ? someOf xs
del x (y:ys) = if x == y then ys else y : del x ys

select xs = sumUp xs (someOf xs)
sumUp xs m = m + sum (del m xs)

sum [] = 0
sum (x:xs) = x + sum xs

{-
KiCS2:
select [1..100]: 6.36  opt: 0.01
-}
