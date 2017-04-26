xorSelf x = slow_id 10000000 (xor x x)

xor True x = not x
xor False x = x

not True = False
not False = True

slow_id n y = if n==0 then y else slow_id (n-1) y

aBool = True ? False


main = xorSelf aBool
