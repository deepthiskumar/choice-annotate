--module GitEncode where

import Prelude as P hiding (readFile, writeFile, appendFile)
import Data.Text.IO as I
import GitDag as G
import CCLibPat
import System.Environment (getArgs)
import Data.Text as T 
import Text.Printf (printf)
import Control.Exception as Exc
import System.Directory (doesFileExist)
--import Data.ByteString.Char8 as B hiding (putStrLn, writeFile, readFile)
import Data.Map as M hiding ((\\))
import Data.Graph.Inductive.Graph
import Control.Monad.Trans.State
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad (foldM)
import CCMerge
import Data.List as L
import Debug.Trace
import GHC.IO.Encoding.UTF16
import GHC.IO.Handle
import System.IO (openFile, IOMode(..))

--TODO
{-
1. get merge base and check if merge has conflicts --Done
 Check sending git executable location. Add a global git config file. Done: Added email and name in the command itself
2. traverse the dag and encode. Done
3. Standalone merges. Done (CC Merge)
4. use ccmerge and merge selections (simple) 
5. for now merge simply (no conflict => serialize, conflict => branch(even if ours or theirs))
6. then check complex merges (check patch in ccMerge)
-}

type FileMap = Map FilePath (Selection, VString)
type RepoInfoMap = Map CommitNode FileMap

type SelState a = StateT RepoInfoMap IO a

main = do
    args <- getArgs
    let repo = P.head args
    let branch = ((P.head).(P.tail)) args
    --create a dag from the commits
    dag <- getDag repo branch
    runGitCmd repo (gitCheckout branch)
    --now for each commit, encode all the files in it
    let ctxt = commitContext dag 0
    execStateT (encodeCommits repo dag ctxt) M.empty
    runGitCmd repo (gitCheckout branch)
    print "Finished Encoding"

 
encodeCommits :: FilePath -> GitDag -> Context Commit () -> SelState ()
encodeCommits repo dag c@(parents,id,commit,children) = do
    continue <- manageCommit repo dag c
    if not continue then return ()
    else do foldM (\_ ctxt -> encodeCommits repo dag ctxt) () (G.children dag id)

manageCommit :: FilePath -> GitDag -> Context Commit () -> SelState Bool
manageCommit repo dag c@(parents,id,commit,children) 
    | isMergeCommit commit = do
        s <- get 
        if allParentsEncoded dag parents s then do 
          liftIO $ print ("Merge commit: "++ show (id,commit));
          liftIO $ runGitCmd repo (gitCheckout (T.unpack $ commitID commit)) 
          mergeCommitFiles repo dag c--merge commits
          return True
        else do
          liftIO $ print ("Merge commit (Incomplete) : "++ show (id,commit))
          return False
    | otherwise            = do
        liftIO $ print ("Non-merge commit : "++ show (id,commit))
        fileList <- liftIO $ runGitCmd repo (gitDiffTree (id, commit))
        liftIO $ runGitCmd repo (gitCheckout (T.unpack $ commitID commit))
        mapM (ccEncode dag repo (id,commit)) (P.map T.unpack fileList)
        return True
   
allParentsEncoded :: GitDag -> Adj () -> RepoInfoMap -> Bool
allParentsEncoded dag ps m = P.and [M.member (commitNode dag p) m | ((),p) <- ps]
  
mergeCommitFiles :: FilePath -> GitDag -> Context Commit () -> SelState ()
mergeCommitFiles repo dag c@(parents,id,com@(Commit h a d (Just m)),children) = do
    --get the changed files in each of the parent
    let cf = changedFiles m
    let fc = M.toList $ comFilesToFileComs cf M.empty
    let rem = L.filter (\(f,_)-> f `L.notElem` (conflictedFiles m)) fc
    let conf = L.filter (\(f,_)-> f `L.elem` (conflictedFiles m)) fc
    mergeAllFiles repo (id,com) dag rem
    mergeConflictedFiles repo (id, com) dag conf
      

mergeAllFiles :: FilePath -> CommitNode -> GitDag -> [(FilePath,[CommitHash])] -> SelState ()
mergeAllFiles _ _ _ []                      = return ()
mergeAllFiles repo mcommit dag ((f,[c]):ms) = do --Required for fast forward merges because the lca is the first parent
   s <- get
   let newSel = lookUpSel dag (nodeFromHash dag c) f s
   put $ updateSel mcommit f newSel s
   liftIO $ writeVFile (repo ++ "/" ++ f) mcommit newSel
   mergeAllFiles repo mcommit dag ms
mergeAllFiles repo mcommit dag ((f,cs):ms)  = do --need to be merged
   let anc = (lca $ (fromJust $ mergeCommit $ snd mcommit))
   if (nodeFromHash dag  anc) `elem` (parent dag (fst mcommit)) then do
     mergeAllFiles repo mcommit dag ((f,cs\\[anc]):ms)
   else do
     newSel <- mergeFile mcommit dag (f,cs)
     liftIO $ writeVFile (repo ++ "/" ++ f) mcommit newSel
     mergeAllFiles repo mcommit dag ms
   
mergeConflictedFiles :: FilePath -> CommitNode -> GitDag -> [(FilePath,[CommitHash])] -> SelState () 
mergeConflictedFiles _ _ _ []                   = return ()
mergeConflictedFiles repo mcommit dag ((f,cs):ms)  = do
   newSel <- mergeFile mcommit dag (f,cs)
   liftIO $ runGitCmd repo (gitCheckout (T.unpack $ commitID $ snd mcommit))
   ccEncode dag repo mcommit f
   mergeConflictedFiles repo mcommit dag ms
   
mergeFile :: CommitNode -> GitDag -> (FilePath,[CommitHash]) -> SelState (Selection, VString)
mergeFile mcommit dag (f,cs)  = do
   s <- get
   --get first parent if present
   let fpt = pickParent mcommit cs
   let sel = latestVS dag (f, cs \\ [fpt]) s
   --get commit from firstParent branch for f
   let p = lookUpSel dag (nodeFromHash dag fpt) f s
   --p should be given the priority hence foldr
   let newSel = P.foldr (merge) p sel
   put $ updateSel mcommit f newSel s
   return newSel  

merge :: (Selection,VString) -> (Selection,VString) -> (Selection,VString)
merge vs vs' 
   | vs == vs' = vs
   | otherwise = mergeVS vs vs'  

   
pickParent :: CommitNode -> [CommitHash] -> CommitHash
pickParent mcommit cs 
    | firstParent 
        (fromJust $ mergeCommit $ snd mcommit) `notElem` cs = L.head cs
    | otherwise  = commitID $ snd mcommit
   
latestVS :: GitDag -> (FilePath,[CommitHash]) -> RepoInfoMap -> [(Selection, VString)]
latestVS _ (f,[]) _     = []
latestVS dag (f,c:cs) m = (lookUpSel dag (nodeFromHash dag c) f m) :
          (latestVS dag (f,cs) m)
   
comFilesToFileComs :: [(CommitHash,[FilePath])] -> Map FilePath [CommitHash] -> Map FilePath [CommitHash]
comFilesToFileComs [] m      = m
comFilesToFileComs (c:cfs) m = comFilesToFileComs cfs (updateFCMap c m)


updateFCMap :: (CommitHash,[FilePath]) -> Map FilePath [CommitHash] -> Map FilePath [CommitHash]
updateFCMap (c,[]) m   = m
updateFCMap (c,f:fs) m = updateFCMap (c,fs) (M.alter (\val -> Just ( c:(fromMaybe [] val))) f m)

  
ccEncode :: GitDag -> FilePath -> CommitNode -> FilePath -> SelState ()
ccEncode dag repo cnode@(dim,commit) f  =
   StateT (\s ->  do
    let file = (repo ++ "/" ++ f)
    print ("In ccTrack block :" ++ file)
    exists <- doesFileExist file
    let target = file
    vexists <- doesFileExist (target++".v")
    if(not exists)
    then do
         print $ "Source " ++ file ++ " doesnt exist"
         return ((),s)
    else do
         source <- readSFile file
         if not vexists 
           then do 
              result <- writeVFile target cnode ([],[Str source])
              return ((),updateSel cnode f ([],[Str $ stripNewline source]) s) 
           else do
              let (sel,vs) = lookUpSel dag cnode f s
              --if dim  > 14 then do
              print "printing intermediate"
              writeVFile (target++".intr") cnode (sel,vs)
              --else do writeVFile (target++".intr") cnode ([],[])
              let dtext = distill (dim) vs sel (stripNewline source)
              writeVFile target cnode ((RSel dim):sel,dtext)
              {-if dim  == 16 then do
                print "printing intermediate 14"
                print dtext
                error "Stop"
              else do print ""-}
              return $ ((),updateSel cnode f ((RSel dim):sel,dtext) s) 
    )   


writeVFile :: FilePath -> CommitNode -> (Selection,VString) -> IO ()
writeVFile f c (_,[])    = return ()
writeVFile f c (sel, vs) = do
  Exc.catch ( writeFile (f++".v") $ (T.pack $ showVText vs)) writeHandler
  mexists <- doesFileExist (f++".m")
  if not mexists then do
    Exc.catch ( writeFile (f++".m") $ (mtemplate `append` serialize c sel)) writeHandler
  else do
    Exc.catch ( appendFile (f++".m") $ (serialize c sel)) writeHandler


mtemplate :: Text
mtemplate = T.pack "COMMIT ID:DIM|DATE|AUTHOR|VIEW DECISION\n"

serialize :: CommitNode -> Selection -> Text
serialize (id, Commit h a d _) sel = 
  h `T.append` 
   (T.pack "|") `T.append` 
    (T.pack $ show id) `T.append`
     (T.pack $ "|") `T.append`
      (showDateTime d) `T.append`
       (T.pack $ "|") `T.append`
        a `T.append`
         (T.pack $ "|") `T.append`
          (T.pack $ show sel) `T.append`
            (T.pack "\n")   


readSFile :: FilePath -> IO Text
readSFile f = Exc.catch (I.readFile f) readHandler >>= return
{-do
    fhandle <- openFile f ReadMode
    hSetEncoding fhandle utf16
    Exc.catch (I.hGetContents fhandle) readHandler >>= return-}
    
lookUpSel :: GitDag -> CommitNode -> FilePath -> RepoInfoMap -> (Selection, VString)
lookUpSel dag cnode f m =
   case lookup' cnode f m of
      ([],[]) -> lookUpParentSel dag cnode f m
      xs      -> xs
             
lookUpParentSel :: GitDag -> CommitNode -> FilePath -> RepoInfoMap -> (Selection, VString)
lookUpParentSel dag cnode f m = 
   case fParent dag (fst cnode) of
     Just p  -> case lookup' p f m of
                  ([],[]) ->lookUpParentSel dag p f m
                  xs -> xs 
     Nothing -> ([],[])


lookup' :: CommitNode -> FilePath -> RepoInfoMap -> (Selection, VString)
lookup' cnode f m = case M.lookup cnode m of
    Just m'  -> case M.lookup f m' of 
                   Just s  -> s
                   Nothing -> ([],[])
    Nothing -> ([],[])
    
updateSel :: CommitNode -> FilePath -> (Selection, VString) -> RepoInfoMap -> RepoInfoMap
updateSel c f sel = M.alter (Just . M.insert f sel . fromMaybe M.empty) c
    
fileMap :: CommitNode -> FilePath -> RepoInfoMap -> FileMap
fileMap cnode f m = case M.lookup cnode m of
    Just m'  -> m'
    Nothing -> M.empty

writeHandler :: IOError -> IO ()
writeHandler e = do 
  P.putStrLn ("Error occurred during write operation: " ++ show e)   
  return ()
  
readHandler :: IOError -> IO Text
readHandler e = do 
  P.putStrLn ("Error occurred during read operation: " ++ show e)   
  return $ T.empty

nextDimensionS :: VString -> Int
nextDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (P.maximum ds) + 1

stripNewline :: Text -> Text
stripNewline s 
  | T.null s          = s
  | T.last s == '\n'  = T.init s
  | otherwise         = s

errorIf :: Bool -> String -> IO ()
errorIf b m = if b then print m else return ()  
           
    
