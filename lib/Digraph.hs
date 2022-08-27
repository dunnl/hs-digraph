module Digraph where

import PP
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

-- | The type of outgoing edges from a node
type NodeMap = Map String Node

data Node =
  Node { -- | Node identifier
        uuid :: String,
         -- | Value stored at node
        value :: Int,
         -- | Neighbors (outgoing edges)
         ns :: NodeMap
       }

-- | Equality on nodes
eqNode :: Node -> Node -> Bool
eqNode (Node i _ _) (Node j _ _) = i == j

instance Eq Node where
  (==) = eqNode

-- | @recursiveShow depth node@ displays
-- conducts a DFS of @node@ and all its neighbors up to depth @depth@,
-- returning a list of strings to be printed.
recursiveShowLines :: Int -> Node -> [String]
recursiveShowLines depth (Node name value ns) =
  ["Node '" ++ name ++ "' (value := " ++ show value ++ ")"] ++
  if depth > 0
  then let ns' = Map.toList ns
           max = maximum (fmap (length . fst) ns')
           arrow p = replicate (max - length p + 4) '.'
           recurse node = recursiveShowLines (depth - 1) node
       in concatMap (\(p, node) -> prependBlock (p ++ arrow p) (recurse node)) ns'
  else []

-- | Default depth when performing DFS on a node
defaultDepth :: Int
defaultDepth = 4

showNode :: Node -> String
showNode = intercalate "\n" . recursiveShowLines defaultDepth

instance Show Node where
  show = showNode
