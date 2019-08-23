module Main where

a = [1, 2, 3, 4]
b = squareArr a
c = pairSum a
d = pairSquaredSum a

-- doSomething :: Int -> String
-- doSomething x
--     | x < 3 = show x ++ " is a little number"
--     | True = show x

squareArr :: [Int] -> [Int]
squareArr (x:[]) = x*x : []
squareArr (x:xs) = x*x : (squareArr xs)

arrToString :: [Int] -> [Char]
arrToString (x:[]) = show x
arrToString (x:xs) = show x ++ ", " ++ (arrToString xs)

pairSum :: [Int] -> [Int]
pairSum [] = []
pairSum (x:y:[]) = (x + y) : []
pairSum (x:y:ys) = (x + y) : (pairSum (y:ys))

pairSquaredSum :: [Int] -> [Int]
pairSquaredSum xs = pairSum (squareArr xs)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1) + fibonacci (n - 2))

listOfFibs :: Int -> [Int]
listOfFibs n = map (fibonacci) [0..n]

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

main :: IO ()
main = do
    putStrLn ("Original: " ++ arrToString a)
    putStrLn ("Squared: " ++ arrToString b)
    putStrLn ("Pair sum: " ++ arrToString c)
    putStrLn ("Pair squared sum: " ++ arrToString d)
    putStrLn ("Reversed: " ++ arrToString (reverseList a))

    putStrLn (arrToString (listOfFibs 10))