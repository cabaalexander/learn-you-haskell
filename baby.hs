doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

enumeratePair xs = zip [1..] xs

