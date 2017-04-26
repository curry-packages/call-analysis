-- Example from Bert/Echahed/Ostvold'93

conc [] ys = ys
conc (x:xs) ys = x : conc xs ys

main xs ys z zs = conc xs (conc ys (z:zs))
--> depth-1 analysis: 4 iterations: main... = :(*,*)

