-- Example to verify correct uses of the partially defined function head
-- via a call pattern analysis

-- list concatenation:
conc [] ys = ys
conc (x:xs) ys = x : conc xs ys

-- append an element at the end of a list:
appLast xs x = conc xs [x]

-- test for null list:
null []     = True
null (x:xs) = False

-- head should be only called with a non-empty list
head (x:_) = x

-- return True (if list is empty) or first list element:
first xs = if null xs then True
                      else head xs

-- generator for arbitrary lists:
genList = []
genList = x : genList where x free

-- main call: call first with the concatenation of arbitrary lists,
-- call head with a last with one element at the end
main x = (first (conc genList genList), head (appLast genList x))

-- a call pattern analysis with depth 1 shows that head is only
-- called with a non-empty list
