-- Example from catch paper by Mitchell/Runciman

main x = tails x

tails x = foldr tails2 [[]] x

tails2 x y = (x:head y) : y

head (x:_) = x
head [] = error ""

foldr            :: (a->b->b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- if the analysis show that (error ...) is not called,
-- it proves that there is no error due to incomplete pattern matching

-- Benchmark data:
-- 15 rules, depth 1, FP size: 31, MFP size: 7
