module Main where

import Data.Char (isSpace)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Console.Haskeline

import Digraph
import Actions

node0 :: Node
node0 = Node "n_zero" 0 $ Map.fromList [("right", node1), ("self", node0)]

node1 :: Node
node1 = Node "n_one" 1 $ Map.fromList [("left", node0), ("right", node2)]

node2 :: Node
node2 = Node "n_two" 2 $ Map.fromList [("left", node1), ("up", node0)]

main :: IO ()
main = loop node0
  where loop node =
          do print node;
             action <- runInputT mySettings getAction
             loop (interpret action node)

getAction :: InputT IO Action
getAction =  do
  minput <- getTrimInputLine "Command: "
  case minput of
    Just "write" ->
      do mvalue <- getTrimInputLine "New value: "
         case mvalue of
           Just value -> return $ Write (read value)
           Nothing -> return $ Write 42
    Just "cut" ->
      do Just new <- getTrimInputLine "Edge to cut: "
         return (DropEdge new)
    Just "follow" ->
      do Just edge <- getTrimInputLine "Edge to follow: "
         return (Follow edge)
    Just "drop" ->
      do Just new <- getTrimInputLine "Edge to merge into: "
         return (AbsorbInto new)
    Just "target" ->
      do Just target <- getTrimInputLine "Targetted node: "
         action <- getAction
         return (Target target action)
    _ ->
      do outputStrLn "I don't recognize this input. Looping."
         getAction

getTrimInputLine :: String -> InputT IO (Maybe String)
getTrimInputLine prompt = fmap (fmap trim) $ getInputLine prompt

getInt :: Int -> String -> IO Int
getInt def prompt =
  do putStrLn $ prompt ++ " (default = " ++ show def ++ "):"
     readLn

wordList = [ "write", "follow", "drop", "cut", "target"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `List.isPrefixOf`) wordList

mySettings :: Settings IO
mySettings = Settings { historyFile = Just "./command_history.txt"
                      , complete = completeWord Nothing " \t" $ return . searchFunc
                      , autoAddHistory = True
                      }

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
