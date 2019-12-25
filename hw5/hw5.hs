--{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b

evalStr :: String -> Maybe Integer
evalStr expr = case parseExp Lit Add Mul expr of
    (Just a) -> Just (eval a)
    Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)    

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

instance Expr Integer where
    lit a = a
    mul = (*)
    add = (+)

instance Expr Bool where
    lit a 
        | (a < 0)   = False
        | otherwise = True
    mul = (&&)
    add = (||)

instance Expr MinMax where
    lit a = MinMax a
    mul (MinMax a) (MinMax b) = MinMax (max a b)
    add (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)

reify :: ExprT -> ExprT
reify = id

reifyInteger :: Integer -> Integer
reifyInteger = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

main :: IO()
main = do
    putStrLn $ show $ testInteger
    putStrLn $ show $ testBool
    putStrLn $ show $ testMM
    putStrLn $ show $ testSat
    
