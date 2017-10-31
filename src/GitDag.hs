module GitDag where

import Data.Graph.Inductive.Tree as T (Gr)
import Data.Graph.Inductive.Graph as G --(mkGraph, insNode, insEdge)
import Data.Graph.Inductive.Example (labUEdges)
import Control.DeepSeq
import Data.Map.Strict as M hiding (map, filter)
import Data.Maybe (fromMaybe)   
import Data.Tuple (swap)
import Lib.Git.Type
import Debug.Trace

type Commit = String
type GitDag = Gr CommitID ()
type CommitNode = LNode CommitID
type CommitEdge = UEdge

type Line = String
type GitLog = [Line]

type Repo = FilePath
type Branch = String

getDAG :: Repo -> Branch -> IO GitDag
getDAG r b = do 
  runGit (makeConfig r Nothing) (gitCheckout b)
  log <- runGit (makeConfig r Nothing) gitlogP
  return $ gitDAG log

gitCheckout :: Branch -> GitCtx ()
gitCheckout b = do
  o <- gitExec "checkout" [b] []
  case o of
      Right out -> return ()
      Left err  -> gitError err ("Error while running git checkout "++b)

--use reverse because mkgraph expects the source to be 
--present in order to add the edge coming from it
gitlogP :: GitCtx GitLog
gitlogP = do
   o <- gitExec "log" ["--pretty=format:%h %p", "--reverse"] []
   case o of
      Right out -> return $ lines out
      Left err  -> gitError err "Error while running git log"


--mkGraph instance of fgl fails if the source node is not present
gitDAG :: GitLog -> GitDag
gitDAG []    = G.empty
gitDAG lines = let (m,es) = buildListnMap lines 0 (M.empty, [])
               in G.mkGraph (toNodeList m) es 

buildListnMap :: GitLog -> Int -> (Map Commit Int, [UEdge]) -> (Map Commit Int, [UEdge])
buildListnMap [] _ p = p
buildListnMap (l:ls) key (nMap, eList) = buildListnMap ls (key+1) (newMap, newList++eList)
    where commit  = head $ take 1 $ words l
          newMap  = M.insert commit key nMap
          parents = drop 1 $ words l
          newList = map (\parent -> (fromMaybe (-1) (M.lookup parent nMap),key,())) parents

toNodeList :: Map Commit Int -> [CommitNode]
toNodeList m = map swap (M.toList m)  

getMergeNodes :: GitDag -> [CommitNode]
getMergeNodes g = filter (\n -> let (p,_,_,_) = context g (fst n) in moreThanTwo p ) (labNodes g)
  where moreThanTwo []  = False
        moreThanTwo [x] = False
        moreThanTwo _   = True


-- Do you really need to check merge conflicts? I guess will help in the merging. But nothing else
-- always compare a commit to its parent and store the selections in commit metadata for each commit
-- 1) When merging for no conflict, simply add the new choices into the latest Vstring
-- 2) When merging with conflicts
--    a) ours   

--git show-branch --merge-base = git merge-base --octopus


-- Examples
--TODO error handling inserting duplicate nodes

un = [1..5]

ns = zip un ["a","b","c","d"]

es = [(1,2),(2,3),(2,4)]

ues = labUEdges es

g :: Gr String ()
g = mkGraph ns ues

--First insert the node and then the edges so that the context (adjacencies) is updated TODO??

--g' = insNode (5,"e") g
g'' = insEdge (3,5,()) (insEdge (4,5,()) (insNode (5,"e") g) )

--Context a b = (list of parents, node, node label, list of children)
