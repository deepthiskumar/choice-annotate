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
import Data.ByteString.Char8 as B hiding (map, filter, head, drop, zip)
import Data.Text as T
import Data.Maybe
import Data.DateTime
import Text.Printf

type CommitHash = ByteString

data Commit = Commit{
    commitID :: CommitHash,
    author :: ByteString,
    date :: DateTime,
    mergeCommit :: Maybe Merge}
      deriving(Show, Eq, Ord)
--First parent is the branch on which othee branch was merged to.
--Knowing the first parent helps to identify which chain of ancestors
--to climb through in order to get to the lca otherwise the inner branches
--could lead to a different lca
data Merge = Merge {
    lca :: CommitHash,
    firstParent :: CommitHash,
    conflictedFiles :: [FilePath]}
      deriving(Show, Eq, Ord)

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
  dag <- gitDAG r log
  printf "Dag for the repo %s created \n" r
  return dag

runGitCmd :: FilePath -> GitCtx a -> IO a
runGitCmd repo command = runGit (makeConfig repo (Just "/usr/bin/git")) command

commitContext :: GitDag -> Node -> Context Commit ()
commitContext = G.context

children :: GitDag -> Int -> [Context Commit ()]
children g n =  [G.context g n'| n' <- ns, isJust (lab g n' ) ]
    where 
      ns = suc g n
      
parent :: GitDag -> Int -> [CommitNode]
parent g n = [(n', fromJust $ lab g n')| n' <- ns, isJust (lab g n' ) ]
    where 
      ns = pre g n
      
fParent :: GitDag -> Int -> Maybe CommitNode
fParent dag n = case lab dag n of
  Just (Commit id a d Nothing)   -> maybe Nothing (Just.(P.head)) (maybeParent dag n)
  Just (Commit id a d (Just m))  -> Just $ matchCommitId (firstParent m) (parent dag n)
  Nothing                        -> error ("CommitNode undefined: "++show n)

--mkGraph instance of fgl fails if the source node is not present
gitDAG :: Repo -> GitLog -> IO GitDag
gitDAG _ []    = return G.empty
gitDAG r lines = do 
   (m,es) <- buildListnMap r lines 0 (M.empty, [])
   return $ G.mkGraph (toNodeList m) es 

-------------------------------------------------------------------------------        
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

{-gitDiffTreeRoot :: CommitHash -> GitCtx GitLog
gitDiffTreeRoot commit = do
    o <- gitExec "diff-tree" 
      ["--no-commit-id", "--name-only", "-r", "--root", B.unpack commit] []
    case o of
      Right out -> return $ B.lines (B.pack out)
      Left err  -> gitError err "Error while running git diff-tree --root"-}
      
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
       
gitMergeBase :: [CommitHash] -> GitCtx CommitHash
gitMergeBase xs  = do
    o <- gitExec "show-branch" ("--merge-base" : P.map B.unpack xs) []
    case o of
      Right out -> return (((B.take 7).(P.head).(B.lines).(B.pack)) out )
      Left err  -> gitError err ("Error while running git show-branch --merge-base")

--for octopus merge to work there should be any conflict. So no need to check that      
gitConflictCheck :: CommitHash -> GitCtx [FilePath]
gitConflictCheck c = do
    o <- gitExec "-c" ["user.email='deepthi.s.kumar8@gmail.com'", "-c",  "user.name='Deepthi S Kumar'","merge", B.unpack c, "--no-commit"] []
    case o of
       Right out -> return []
       Left err  -> trace (show err) getConflictedFiles --(return $ parseConflictLog msg)
       
getConflictedFiles :: GitCtx [FilePath]
getConflictedFiles = do
   o <- gitExec "diff" ["--name-only", "--diff-filter=U"] []
   case o of
     Right out -> (return $ P.lines out)
     Left err  -> gitError err ("Error while running git diff --name-only --diff-filter=U")
     
gitAbort :: GitCtx ()
gitAbort = do 
   o <- gitExec "merge" ["--abort"] []
   case o of
      Right out -> return ()
      Left err  -> gitError err ("Error while running git merge --abort")
    
result :: String -> Either GitFailure String -> GitCtx GitLog
result command o = do
    case o of
      Right out -> return $ B.lines (B.pack out)
      Left err  -> gitError err ("Error while running git " ++ command)



-------------------------------------------------------------------------------
--Helpers
      
isRoot x = x == 0

buildListnMap :: Repo -> GitLog -> Int -> (Map CommitHash CommitNode, [UEdge]) -> IO (Map CommitHash CommitNode, [UEdge])
buildListnMap _ [] _ p = return p
buildListnMap r (l:ls) key (nMap, eList) = do
  let (commit,author,date,parents) = parseLog l
  let newList = P.map (\parent -> ((fst $ fromJust $ M.lookup parent nMap),key,())) parents
  if P.length parents > 1 then do
    runGitCmd r (gitCheckout (B.unpack $ P.head parents))
    lca <- runGitCmd r (gitMergeBase parents)
    files <- runGitCmd r (gitConflictCheck (P.head $ P.tail parents))
    runGitCmd r (gitAbort)
    let merge = Just (Merge lca (P.head parents) files)
    let newMap  = M.insert commit (newCommitNode key commit author date merge) nMap
    buildListnMap r ls (key+1) (newMap, newList++eList)
  else do
    let newMap  = M.insert commit (newCommitNode key commit author date Nothing) nMap
    buildListnMap r ls (key+1) (newMap, newList++eList)
  
  
    {-where (commit,author,date,parents) = parseLog l--P.head $ P.take 1 $ B.words l
          newMap  = M.insert commit (commitNode key commit author date Nothing) nMap --parents = P.drop 1 $ B.words l
          newList = P.map (\parent -> ((fst $ fromJust $ M.lookup parent nMap),key,())) parents
          --update merge details as well -}

toNodeList :: Map CommitHash CommitNode -> [CommitNode]
toNodeList m = P.map snd (M.toList m)  

dateFormat = "%F"

parseLog :: ByteString -> (CommitHash, ByteString, DateTime, [CommitHash])
parseLog s = case B.split '|' s of
    c:p:a:d:[] -> (c,a,fromJust $ parseDateTime dateFormat (B.unpack d),B.words p)
    otherwise  -> error ("commit log parsing error: "++ show s) 

newCommitNode :: Node -> CommitHash -> ByteString -> DateTime -> Maybe Merge -> CommitNode
newCommitNode n c a d m = (n, Commit c a d m)

--TODO get conflicted files of the merge nodes
getMergeNodes :: GitDag -> [CommitNode]
getMergeNodes g = P.filter (\n -> let (p,_,_,_) = context g (fst n) in moreThanTwo p ) (labNodes g)
  where moreThanTwo []  = False
        moreThanTwo [x] = False
        moreThanTwo _   = True
        


maybeParent :: GitDag -> Int -> Maybe [CommitNode]
maybeParent dag n = case parent dag n of
  [] -> Nothing
  xs -> Just xs 

matchCommitId :: CommitHash -> [CommitNode] -> CommitNode
matchCommitId c [] = error ("No commit nodes to match "++ B.unpack c)
matchCommitId c (p@(_,(Commit c' _ _ _)):ps) 
   |c == c'  = p
   |otherwise  = matchCommitId c ps

isMergeCommit :: Commit -> Bool
isMergeCommit (Commit _ _ _ (Just _)) = True
isMergeCommit _                       = False

commitNode :: GitDag -> Node ->  CommitNode
commitNode dag n = case lab dag n of
    Just l   -> (n,l)
    Nothing  -> error ("Commit node not found for node id:" ++ show n)

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
