{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module CreditCard where

toDigits :: Integer -> [Integer]
toDigits n 
    | n > 0 = (toDigits (n `div` 10)) ++ [n `mod` 10]
    | otherwise = []

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList(xs) ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverseList (toDigits n)

arrToString :: [Integer] -> [Char]
arrToString [] = ""
arrToString (x:[]) = show x
arrToString (x:xs) = show x ++ ", " ++ (arrToString xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (reverseList -> (x:y:zs)) = doubleEveryOther (reverse zs) ++ [y*2, x]
doubleEveryOther (x:[]) = [x]
doubleEveryOther _ = []

sumDigits :: [Integer] -> Integer
sumDigits (x:[]) = x
sumDigits (intArrToDigitsArr -> x:xs) = x + sumDigits(xs)

intArrToDigitsArr :: [Integer] -> [Integer]
intArrToDigitsArr (x:[]) = toDigits x ++ []
intArrToDigitsArr (x:xs) = (toDigits x) ++ (intArrToDigitsArr xs)



a, b, c, d, e, f :: Bool
g :: Bool
a = toDigits 1234 == [1,2,3,4]
b = toDigitsRev 1234 == [4,3,2,1]
c = toDigits 0 == []
d = toDigits (-17) == []
e = doubleEveryOther [8,7,6,5] == [16,7,12,5]
f = doubleEveryOther [1,2,3] == [1,4,3]
g = sumDigits [16,7,12,5] == 22

main :: IO ()
main = do
    putStrLn (show a)
    putStrLn (show b)
    putStrLn (show c)
    putStrLn (show d)
    putStrLn (show e)
    putStrLn (show f)
    putStrLn (show g)
