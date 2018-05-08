doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

enumeratePair xs = zip [1..] xs

removeNonUppercase :: String -> String
removeNonUppercase xs = [x | x <- xs, x `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

isItSeven :: Int -> String
isItSeven 7 = "Yes mate, it is a seven! :'V"
isItSeven x = "Nope, it is not seven my friend."

