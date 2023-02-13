infixl 1 `bind`,`seq`

conc [] ys = ys
conc (x:xs) ys = x : conc xs ys

anyval = failed

getChar w = (anyval,w)

getLine w = (anyval,w)

putChar _ w = ((),w)

readFile _ w = (anyval,w)

tmpDir = "/tmp/"

readTmpFile c = readFile (conc tmpDir [c])

data World = W

bind a1 a2 w = bind1 (a1 w) a2
bind1 (r,w) a = a r w

seq a1 a2 w = seq1 (a1 w) a2
seq1 (_,w) a = a w

--main = bind getChar putChar W
--main = (putChar 0 `seq` putChar 1 `seq` putChar 2) W
--main = (getChar `bind` readFile `seq` putChar) W
--main w = bind getLine readTmpFile w
main w = bind getChar readTmpFile w

-- depth-8 analysis:
--> readFile(:(/,:(t,:(m,:(p,:(/,:(*,[])))))),*) = (,)(*,*)
