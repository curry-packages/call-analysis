conc [] ys = ys
conc (x:xs) ys = x : conc xs ys

add12 xs = conc xs [1,2]

oneElem [] = False
oneElem [_] = True
oneElem (_:_:_) = False

main = oneElem (add12 xs) where xs free
