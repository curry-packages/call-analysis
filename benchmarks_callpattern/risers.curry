-- Example from catch paper by Mitchell/Runciman

risers [] = []
risers [x] = [[x]]
risers (x:y:etc) = risers2 (leq x y) x (risers (y:etc))

risers2 b x (s:ss) = risers3 b x s ss
risers2 _ _ [] = error "" -- match error

risers3 True  x s ss = (x:s) : ss
risers3 False x s ss = [x] : (s:ss)

--main = risers [1,2,3,1,2]
main x = risers x

--leq x y = x <= y
leq _ _ = failed

-- if the analysis show that (error ...) is not called,
-- it proves that there is no error due to incomplete pattern matching

-- Benchmark data:
-- 9 rules, depth 1, FP size: 13, MFP size: 9
