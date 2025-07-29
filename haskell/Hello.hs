x :: Int
-- x = x + 1

x = maxBound

reallyBig :: Integer
reallyBig = 2 ^ 107

-- Basic funcions

sumtorial :: Int -> Int
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

-- iusing guards
hailstone :: Integer -> Integer
hailstone n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

-- List
k :: [Int]
k = 3 : 2 : []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq n
  | (n == 1) = [1]
  | otherwise = n : hailstoneSeq (hailstone n)

-- functions on list
getListLength :: [Integer] -> Integer
getListLength [] = 0
getListLength (x : xs) = 1 + getListLength xs

main = do
  putStrLn "hello world"
  print x
  print reallyBig
  print k
  print "hello"
  print [1, 2, 3]
