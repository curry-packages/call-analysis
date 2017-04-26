conc [] ys = ys
conc (x:xs) ys = x : conc xs ys

appLast xs x = conc xs [x]

null [] = True
null (_:_) = False

main xs = null (appLast xs [1])
