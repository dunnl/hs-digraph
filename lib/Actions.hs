module Actions where

import Data.Map (Map)
import qualified Data.Map as Map

import Digraph

data Action =
  InsertEdgeTo String Node
  | InsertFreshNode String String Int
  | Write Int
  | Follow String
  | AbsorbInto String
  | DropEdge String
  | Target String Action

replaceNodeWith :: String -> Node -> Node -> Node
replaceNodeWith target new (Node name v ns) =
  if name == target then new
  else let this = Node name v (Map.map (replaceNodeWith target new
                                        . replaceNodeWith name this) ns)
  in this

interprets :: [Action] -> Node -> Node
interprets [] node = node
interprets (act : rest) node =
  interprets rest (interpret act node)

interpret :: Action -> Node -> Node
interpret (InsertEdgeTo edge target) node =
  act_insedge edge target node
interpret (DropEdge edge) node =
  act_dropedge edge node
interpret (InsertFreshNode edge name value) node =
  act_insnew edge name value node
interpret (Write new) node =
  act_write new node
interpret (Follow edge) node =
  act_follow edge node
interpret (AbsorbInto edge) node =
  act_absorb edge node
interpret (Target target act) node =
  act_target target act node

-- | Target node for an action
act_target :: String -> Action -> Node -> Node
act_target target act node@(Node name value ns) =
  let this = act_target target act node in
    if name == target
    then interpret act node
    else Node name value
    (Map.map (act_target target act . replaceNodeWith name this) ns)

-- | Create a new edge to an existing node
act_insedge :: String -> Node -> Node -> Node
act_insedge edge new (Node name value ns) =
  let this = act_insedge edge new (Node name value ns)
      newns =  Map.map (replaceNodeWith name this) ns
  in Node name value (Map.insert edge new newns)

-- | Drop an existing edge
act_dropedge :: String -> Node -> Node
act_dropedge edge (Node name value ns) =
  let this = act_dropedge edge (Node name value ns)
      newns =  Map.map (replaceNodeWith name this) ns
  in Node name value (Map.delete edge newns)

-- | Create a new (neighborless) node and insert it
act_insnew :: String -> String -> Int -> Node -> Node
act_insnew edge name value (Node i v ns) =
  let this = act_insnew edge name value (Node i v ns)
      newns =  Map.map (replaceNodeWith i this) ns
      freshNode = Node name value Map.empty
  in Node i v (Map.insert edge freshNode newns)

-- | Follow an edge
act_follow :: String -> Node -> Node
act_follow path node@(Node _ _ ns) =
  maybe node id (Map.lookup path ns)

-- | Overwrite the value of
act_write :: Int -> Node -> Node
act_write new (Node name value ns) =
  let this = Node name new (Map.map (replaceNodeWith name this) ns)
  in this

act_absorb :: String -> Node -> Node
act_absorb str node@(Node name0 value0 ns0)
  = case act_follow str node of
      (Node name1 value1 ns1) ->
        let this =  Node name1 value1
              (Map.map (replaceNodeWith name0 this .
                       replaceNodeWith name1 this) ns1)
        in this
