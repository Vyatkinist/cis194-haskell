{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

a :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
a f = fmap (first f)

b :: Functor f => (a1 -> b) -> (a2 -> f (a1, c)) -> a2 -> f (b, c)
b f g = (fmap (first f)) . g

c :: (a -> b) -> Parser a -> String -> Maybe (b, String)
c f (Parser rp) = ((fmap (first f)) . rp)

d :: (a -> b) -> Maybe a -> Maybe b
d a (Just b) = fmap a (Just b)

instance Functor Parser where
  fmap f (Parser pf) = Parser $ ((fmap (first f)) . pf)

e :: Parser a -> String -> Maybe (a, String)
e = runParser

-- instance Applicative Parser where
--   pure p = Parser $ \str -> Just (p, str)
--   p1 <*> p2 = Parser $ \str -> 
--     case runParser p1 str of
--       Just (o1, rest) -> Just (o1, rest)
--       Nothing -> Nothing


newtype Test = TestConstructor { testFunc :: String -> Int }




















-- instance Functor Parser where
--   fmap f (Parser pf) = Parser (fmap (first f) . pf)

-- instance Applicative Parser where
--   pure a = Parser f
--     where f str = Just (a, str)
--   p1 <*> p2 = Parser f
--     where f str = Just (p1 str, str)
