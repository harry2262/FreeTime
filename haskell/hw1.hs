toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = x `mod` 10 : toDigits (x `div` 10)

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList [x] = [x]
reverseList (x : xs) = reverseList xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x
  | even (length x) = doubleEveryOtherEven x
  | otherwise = doubleEveryOtherOdd x

doubleEveryOtherOdd :: [Integer] -> [Integer]
doubleEveryOtherOdd [x] = [x]
doubleEveryOtherOdd (x : (y : ys)) = x : (2 * y : doubleEveryOtherOdd ys)

doubleEveryOtherEven :: [Integer] -> [Integer]
doubleEveryOtherEven [] = []

doublEveryOtherEven (x : (y : ys)) = (2 * x : (y : doubleEveryOtherEven ys))

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : xs) = x + sumList xs

sumListDigit :: [Integer] -> Integer
sumListDigit [] = 0
sumListDigit (x : xs) = sumList (toDigits x) + sumListDigit xs

validate :: Integer -> Bool
validate x = sumListDigit (doubleEveryOther (reverseList (toDigits x))) `mod` 10 == 0
