{-# LANGUAGE FlexibleInstances #-}

module HW8 where

import Data.Monoid
import Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp { empFun = eFun } (GL list fun) = GL (e:list) (fun + eFun)

instance Monoid GuestList where
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)
    mempty = GL [] 0

instance Semigroup GuestList where
    (<>) = mappend

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 >= l2 then l1 else l2

-- Exercise 2

employee = Emp { empName = "Matthew", empFun = 10 }
empList = GL [Emp "Joe" 5, Emp "John" 15] 20
empList1 = GL [Emp "Norman" 200, Emp "Polly" 100] 300

main :: IO()
--main = putStrLn "Test" >> (readLn >>= \s -> putStrLn s)


main = do
    print $ glCons employee empList
    print $ empList <> empList1
    print $ empList `moreFun` empList1