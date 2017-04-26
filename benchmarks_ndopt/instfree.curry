-- Benchmark: generate list of free variables,
-- instantiate them to strings and use them and duplicates

genFrees n = if n==0 then [] else x : genFrees (n-1)
 where x free

inst n = "adadadadadadadaadad" +++ show n

instFrees _ [] = True
instFrees n (x:xs) | x=:=inst n = instFrees (n+1) xs

[] +++ ys = ys
(x:xs) +++ ys = x : xs+++ys

zs ++++ ys =
 if zs==[] then ys
           else let (x:xs) = zs in x : x : xs++++ys

len [] = 0
len (_:xs) = 1 + len xs

concat [] = []
concat (x:xs) = x +++ concat xs

--useVars True vars = len (concat (vars+++vars+++vars))
useVars True n vars =
  if n==0 then []
  else let (v:vs) = vars
        in v:vs++++(v:vs)++++useVars True (n-1) vars

main n = let vars = genFrees n
          in len (concat (useVars (instFrees 0 vars) 6 vars))
