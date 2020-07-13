{-# LANGUAGE FlexibleInstances #-}

module FunctorExperiments where

--import Data.Functor

a :: Maybe String
a = Just "123"
b :: Maybe String
b = Just "abc"
c :: Maybe String
c = Nothing 

-- instance Functor Maybe where
--     fmap f (Just a) = Just (f a)
--     fmap _ Nothing = Nothing

-- instance Functor [] where
--     fmap f [] = []
--     fmap f (x:xs) = (f x) : fmap f xs

-- instance Functor IO where
--     fmap f ioa = ioa >>= (\x -> return (f x))

main :: IO()
main = do 
    print "Test"