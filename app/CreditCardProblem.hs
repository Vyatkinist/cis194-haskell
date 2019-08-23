{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module CreditCard where

toDigits :: Int -> [Int]
toDigits n 
    | n > 0 = (toDigits (n `div` 10)) ++ [n `mod` 10]
    | otherwise = []

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList(xs) ++ [x]

toDigitsRev :: Int -> [Int]
toDigitsRev n = reverseList (toDigits n)

arrToString :: [Int] -> [Char]
arrToString (x:[]) = show x
arrToString (x:xs) = show x ++ ", " ++ (arrToString xs)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther (reverseList -> (x:y:zs)) = doubleEveryOther (reverse zs) ++ [y*2, x]
doubleEveryOther (x:[]) = [x]
doubleEveryOther _ = []

a = toDigits 1234 == [1,2,3,4]
b = toDigitsRev 1234 == [4,3,2,1]
c = toDigits 0 == []
d = toDigits (-17) == []
e = doubleEveryOther [8,7,6,5] == [16,7,12,5]
f = doubleEveryOther [1,2,3] == [1,4,3]

main :: IO ()
main = do
    putStrLn (show a)
    putStrLn (show b)
    putStrLn (show c)
    putStrLn (show d)
    putStrLn (show e)
    putStrLn (show f)

    putStrLn(arrToString(doubleEveryOther [1, 2, 3]))