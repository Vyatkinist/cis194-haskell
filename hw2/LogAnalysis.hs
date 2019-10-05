{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
module LogAnalysis where
    
import Prelude
import Log

parseMessage :: String -> LogMessage
parseMessage x = case words x of
    ("I":timestamp:xs)              -> LogMessage Info (read timestamp) (unwords xs)
    ("E":errorcode:timestamp:xs)    -> LogMessage (Error $ read errorcode) (read timestamp) (unwords xs)
    ("W":timestamp:xs)              -> LogMessage Warning (read timestamp) (unwords xs)
    wrongMsg                        -> Unknown (unwords wrongMsg)

parse :: String -> [LogMessage]
parse x = map parseMessage $ lines x

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ tstamp1 _) msgtree@(Node nodeLeft msg2@(LogMessage _ tstamp2 _) nodeRight) 
  | tstamp1 == tstamp2 = msgtree
  | tstamp1 < tstamp2 = Node (insert msg1 nodeLeft) msg2 nodeRight
  | tstamp1 > tstamp2 = Node nodeLeft msg2 (insert msg1 nodeRight)

build :: [LogMessage] -> MessageTree
build (x:xs) = insert x (build xs)
build [] = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map getMessage (filter isSevere ms)
  where isSevere (LogMessage (Error s) _ _) = s > 50
        isSevere _ = False
        getMessage (LogMessage _ _ m) = m

tree :: MessageTree
tree = Node Leaf (LogMessage Info 123 "kokoko") Leaf

tree1 :: MessageTree
tree1 = Node Leaf (LogMessage Warning 2 "Test") tree

messages :: IO [LogMessage]
messages = testParse (inOrder . build . parse) 10 "error.log"

errors :: IO [String]
errors = testWhatWentWrong parse whatWentWrong "sample.log"

main :: IO()
main = do
    putStrLn . show $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
    putStrLn . show $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    putStrLn . show $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
    putStrLn . show $ parseMessage "I 23 lalala"
    putStrLn . show $ parseMessage "W 23 lalala"
    putStrLn . show $ tree
    putStrLn . show $ tree1
    putStrLn . show $ (insert (LogMessage Info 543 "ULULU") tree1)
    print =<< messages
    print =<< errors
