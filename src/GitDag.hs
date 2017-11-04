module GitDag where

import Prelude as P
import Data.Graph.Inductive.Tree as T (Gr)
import Data.Graph.Inductive.Graph as G --(mkGraph, insNode, insEdge)
import Data.Graph.Inductive.Example (labUEdges)
import Control.DeepSeq
import Data.Map.Strict as M hiding (map, filter)
import Data.Maybe (fromMaybe)   
import Data.Tuple (swap)
import Lib.Git.Type hiding (Commit)
import Debug.Trace
--import Data.ByteString hiding (map, filter, head, take, drop, zip)
import Data.ByteString.Lazy.Char8 as B hiding (map, filter, head, take, drop, zip)
import Data.Text as T
import Data.Maybe
import Data.DateTime

type CommitHash = ByteString

data Commit = Commit{
    commitID :: CommitHash,
    author :: ByteString,
    date :: DateTime,
    mergeCommit :: Maybe Merge}
      deriving(Show)

data Merge = Merge {
    lca :: (Node, ByteString),
    conflictedFiles :: [FilePath]}
      deriving(Show)

type GitDag = Gr Commit ()
type CommitNode = LNode Commit
type CommitEdge = UEdge

type Line = ByteString
type GitLog = [Line]

type Repo = FilePath
type Branch = String

getDag :: Repo -> Branch -> IO GitDag
getDag r b = do 
  runGitCmd r (gitCheckout b)
  log <- runGitCmd r gitlogP
  return $ gitDAG log

runGitCmd :: FilePath -> GitCtx a -> IO a
runGitCmd repo command = runGit (makeConfig repo Nothing) command

commitContext :: GitDag -> Node -> Context Commit ()
commitContext = G.context

children :: GitDag -> Int -> [CommitNode]
children g n =  [(n',fromJust (lab g n' ))| n' <- ns, isJust (lab g n' ) ]
    where 
      ns = suc g n
      

--mkGraph instance of fgl fails if the source node is not present
gitDAG :: GitLog -> GitDag
gitDAG []    = G.empty
gitDAG lines = let (m,es) = buildListnMap lines 0 (M.empty, [])
               in G.mkGraph (toNodeList m) es 

buildListnMap :: GitLog -> Int -> (Map CommitHash CommitNode, [UEdge]) -> (Map CommitHash CommitNode, [UEdge])
buildListnMap [] _ p = p
buildListnMap (l:ls) key (nMap, eList) = buildListnMap ls (key+1) (newMap, newList++eList)
    where (commit,author,date,parents) = parseLog l--P.head $ P.take 1 $ B.words l
          newMap  = M.insert commit (commitNode key commit author date Nothing) nMap --parents = P.drop 1 $ B.words l
          newList = P.map (\parent -> ((fst $ fromJust $ M.lookup parent nMap),key,())) parents

toNodeList :: Map CommitHash CommitNode -> [CommitNode]
toNodeList m = P.map snd (M.toList m)  

dateFormat = "%F"

parseLog :: ByteString -> (CommitHash, ByteString, DateTime, [CommitHash])
parseLog s = case B.split '|' s of
    c:p:a:d:[] -> (c,a,fromJust $ parseDateTime dateFormat (B.unpack d),B.words p)
    otherwise  -> error ("commit log parsing error: "++ show s) 

commitNode :: Node -> CommitHash -> ByteString -> DateTime -> Maybe Merge -> CommitNode
commitNode n c a d m = (n, Commit c a d m)

--TODO get conflicted files of the merge nodes
getMergeNodes :: GitDag -> [CommitNode]
getMergeNodes g = P.filter (\n -> let (p,_,_,_) = context g (fst n) in moreThanTwo p ) (labNodes g)
  where moreThanTwo []  = False
        moreThanTwo [x] = False
        moreThanTwo _   = True
        
--Git commands
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
   o <- gitExec "log" ["--pretty=format:%h|%p|%an|%cd", "--date=short", "--reverse"] []
   result "log" o

gitDiffTreeRoot :: CommitHash -> GitCtx GitLog
gitDiffTreeRoot commit = do
    o <- gitExec "diff-tree" 
      ["--no-commit-id", "--name-only", "-r", "--root", B.unpack commit] []
    case o of
      Right out -> return $ B.lines (B.pack out)
      Left err  -> gitError err "Error while running git diff-tree --root"
      
gitDiffTree :: CommitNode -> GitCtx GitLog
gitDiffTree (n,c) = do
    if isRoot n then do
       o <- gitExec "diff-tree" 
         ["--no-commit-id", "--name-only", "-r", "--root", B.unpack $ commitID c] []
       result "diff-tree --root" o
    else do
       o <- gitExec "diff-tree" 
         ["--no-commit-id", "--name-only", "-r", B.unpack $ commitID c] []
       result "diff-tree" o
    
result :: String -> Either GitFailure String -> GitCtx GitLog
result command o = do
    case o of
      Right out -> return $ B.lines (B.pack out)
      Left err  -> gitError err ("Error while running git " ++ command)
      
isRoot x = x == 0

--Algorithm to encode
--1. Start with the root
--2. Encode untill a branch occurs (i.e., the node will have two children)
--3. When a a node has more than one child, encode both the first branch and then 


-- Do you really need to check merge conflicts? I guess will help in the merging. But nothing else
-- always compare a commit to its parent and store the selections in commit metadata for each commit
-- 1) When merging for no conflict, simply add the new choices into the latest Vstring
-- 2) When merging with conflicts
--    a) ours   


--TODO notes
--git show-branch --merge-base = git merge-base --octopus
{- author date vs commit date and author vs committer
You may be wondering what the difference is between author and committer. The author is the person who originally wrote the patch, whereas the committer is the person who last applied the patch. So, if you send in a patch to a project and one of the core members applies the patch, both of you get credit — you as the author and the core member as the committer
-}


-- Examples
--TODO error handling inserting duplicate nodes

{-un = [1..5]

ns = zip un ["a","b","c","d"]

es = [(1,2),(2,3),(2,4)]

ues = labUEdges es

g :: Gr String ()
g = mkGraph ns ues

--First insert the node and then the edges so that the context (adjacencies) is updated TODO??

--g' = insNode (5,"e") g
g'' = insEdge (3,5,()) (insEdge (4,5,()) (insNode (5,"e") g) )

--Context a b = (list of parents, node, node label, list of children)-}
