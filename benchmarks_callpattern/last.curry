-- Example from catch paper by Mitchell/Runciman

main x xs = last (x:xs)

last [] = error ""
last [x] = x
last (_:x:xs) = last (x:xs)

-- if the analysis show that (error ...) is not called,
-- it proves that there is no error due to incomplete pattern matching
-- moreover, the first rule of last can be omitted

-- Benchmark data:
-- 4 rules, depth 1, FP size: 4, MFP size: 2
