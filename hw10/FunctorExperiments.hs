{-# LANGUAGE FlexibleInstances #-}

module FunctorExperiments where

--import Data.Functor


import Control.Applicative

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



type Name = String
data Employee = Employee { name :: Name, phone :: String }
  deriving (Show)

maybeEmployee :: Maybe Name -> Maybe String -> Maybe Employee
maybeEmployee Nothing _ = Nothing
maybeEmployee _ Nothing = Nothing
maybeEmployee (Just name) (Just phone) = Just (Employee name phone)



main :: IO()
main = do
    print $ maybeEmployee Nothing (Just "+434234234")
    print $ maybeEmployee (Just "John") Nothing
    print $ maybeEmployee (Just "John") (Just "+42390402394")
    print $ ((1+) <$> (Just 1))
    print $ liftA (+1) (Just 4)
    print $ Just (+1) <*> Just 5
    print $ [(+1), (*2)] <*> [1, 2, 3]
