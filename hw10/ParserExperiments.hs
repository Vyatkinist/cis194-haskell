module ParserExperiments where

import Control.Applicative

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd a b = pure (+) <*> a <*> b

newtype Parser i o = Parser { runParser :: i -> (Maybe o, i) }


main :: IO()
main = do
    putStrLn "Test"
    print $ [(+), (*)] <*> [1, 2] <*> [3, 4]
