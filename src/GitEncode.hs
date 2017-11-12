--module GitEncode where

import Prelude as P hiding (readFile, writeFile)
import Data.Text.IO as I
import GitDag as G
import CCLibPat
import System.Environment (getArgs)
import Data.Text as T 
import Text.Printf (printf)
import Control.Exception as Exc
import System.Directory (doesFileExist)
import Data.ByteString.Char8 as B hiding (putStrLn, writeFile, readFile)
import Data.Map as M hiding ((\\))
import Data.Graph.Inductive.Graph
import Control.Monad.Trans.State
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad (foldM)
import CCMerge
import Data.List as L

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
    {-cont <- readFile "list.txt.v"
    I.putStrLn cont
    again <- readFile "list.txt.v"
    I.putStrLn again-}

 
encodeCommits :: FilePath -> GitDag -> Context Commit () -> SelState ()
encodeCommits repo dag c@(parents,id,commit,children) = do
    continue <- manageCommit repo dag c
    if not continue then return ()
    else do
      foldM (\_ ctxt -> encodeCommits repo dag ctxt) () (G.children dag id)
      --st <- get
      --liftIO $ print (show st)

manageCommit :: FilePath -> GitDag -> Context Commit () -> SelState Bool
manageCommit repo dag c@(parents,id,commit,children) 
    | isMergeCommit commit = do
        s <- get 
        if allParentsEncoded dag parents s then do 
          liftIO $ print ("Merge commit: "++ show (id,commit));
          liftIO $ runGitCmd repo (gitCheckout (B.unpack $ commitID commit)) 
          mergeCommitFiles repo dag c--merge commits
          return True
        else do
          liftIO $ print ("Merge commit (Incomplete) : "++ show (id,commit))
          return False
    | otherwise            = do
        liftIO $ print ("Non-merge commit : "++ show (id,commit))
        fileList <- liftIO $ runGitCmd repo (gitDiffTree (id, commit))
        liftIO $ runGitCmd repo (gitCheckout (B.unpack $ commitID commit))
        mapM (ccEncode dag repo (id,commit) (parent dag id) ) (P.map B.unpack fileList)
        --x <- ccEncodeFiles dag repo (id,commit) (parent dag id) (P.map B.unpack fileList)
        --liftIO $ P.print x
        return True
   
allParentsEncoded :: GitDag -> Adj () -> RepoInfoMap -> Bool
allParentsEncoded dag ps m = P.and [M.member (commitNode dag p) m | ((),p) <- ps]
  
mergeCommitFiles :: FilePath -> GitDag -> Context Commit () -> SelState ()
mergeCommitFiles repo dag c@(parents,id,com@(Commit h a d (Just m)),children) = do
    --get the changed files in each of the parent
    let cf = changedFiles m
    let fc = M.toList $ tobeMerged cf M.empty
    let rem = L.filter (\(f,_)-> f `L.notElem` (conflictedFiles m)) fc
    let conf = L.filter (\(f,_)-> f `L.elem` (conflictedFiles m)) fc
    mergeAllFiles repo (id,com) dag rem
    mergeConflictedFiles repo (id, com) dag conf
      

mergeAllFiles :: FilePath -> CommitNode -> GitDag -> [(FilePath,[CommitHash])] -> SelState ()
mergeAllFiles _ _ _ []                      = return ()
mergeAllFiles repo mcommit dag ((f,[c]):ms) = do --simply find the latest VS and add it to the state 
   s <- get
   let sel = lookUpSel dag (nodeFromHash dag c) f s
   put $ updateSel mcommit f sel s
   let target = (repo ++ "/" ++ f++".v")
   liftIO $ writeVFile target sel--Exc.catch ( writeFile target $ (T.pack $ showVText (snd sel))) writeHandler
   mergeAllFiles repo mcommit dag ms
mergeAllFiles repo mcommit dag ((f,cs):ms)  = do --need to be merged
   newSel <- mergeFile mcommit dag (f,cs)
   let target = (repo ++ "/" ++ f++".v")
   liftIO $ writeVFile target newSel--Exc.catch ( writeFile target $ (T.pack $ showVText (snd newSel))) writeHandler
   mergeAllFiles repo mcommit dag ms
   
mergeConflictedFiles :: FilePath -> CommitNode -> GitDag -> [(FilePath,[CommitHash])] -> SelState () 
mergeConflictedFiles _ _ _ []                   = return ()
mergeConflictedFiles repo mcommit dag ((f,cs):ms)  = do
   newSel <- mergeFile mcommit dag (f,cs)
   let target = (repo ++ "/" ++ f++".v")
   liftIO $ Exc.catch ( writeFile target $ (T.pack $ showVText (snd newSel))) writeHandler
   mergeConflictedFiles repo mcommit dag ms
   
mergeFile :: CommitNode -> GitDag -> (FilePath,[CommitHash]) -> SelState (Selection, VString)
mergeFile mcommit dag (f,cs)  = do
   s <- get
   --get first parent
   let fpt = firstParent $ (fromJust $ mergeCommit $ snd mcommit)
   let sel = latestVS dag (f, cs \\ [fpt]) s
   --get commit from firstParent branch for f
   let p = lookUpSel dag (nodeFromHash dag fpt) f s
   --foldr
   let newSel = P.foldr (mergeVS) p sel
   put $ updateSel mcommit f newSel s
   return newSel   
   
latestVS :: GitDag -> (FilePath,[CommitHash]) -> RepoInfoMap -> [(Selection, VString)]
latestVS _ (f,[]) _     = []
latestVS dag (f,c:cs) m = (lookUpSel dag (nodeFromHash dag c) f m) :
          (latestVS dag (f,cs) m)
   
tobeMerged :: [(CommitHash,[FilePath])] -> Map FilePath [CommitHash] -> Map FilePath [CommitHash]
tobeMerged [] m      = m
tobeMerged (c:cfs) m = tobeMerged cfs (updateCommits c m)


updateCommits :: (CommitHash,[FilePath]) -> Map FilePath [CommitHash] -> Map FilePath [CommitHash]
updateCommits (c,[]) m   = m
updateCommits (c,f:fs) m = updateCommits (c,fs) (M.alter (\val -> Just ( c:(fromMaybe [] val))) f m)


ccEncodeFiles :: GitDag -> FilePath -> CommitNode -> [CommitNode] ->[FilePath] -> SelState ()
ccEncodeFiles _ _ _ _ []                    = return ()
ccEncodeFiles dag repo cnode parents (f:fs) = do
   ccEncode dag repo cnode parents f
   ccEncodeFiles dag repo cnode parents fs
  
ccEncode :: GitDag -> FilePath -> CommitNode -> [CommitNode] -> FilePath -> SelState ()
ccEncode dag repo cnode@(dim,commit) parents f  = do
   StateT (\s ->  do
    --print (show s) 
    let file = (repo ++ "/" ++ f)
    print ("In ccTrack block :" ++ file)
    exists <- doesFileExist file
    let target = file ++ ".v"
    vexists <- doesFileExist target
    if(not exists)
    then do
         print $ "Source " ++ file ++ " doesnt exist"
         return ((),s)
    else do
         source1 <- try (readFile file) :: IO (Either SomeException (Text))
         --source <- S.readFile file
         case source1 of
            Left ex -> do 
                 P.putStrLn $ "Caught exception while reading the source file: " ++ show ex
                 return ((),s)
            Right source -> do
              if not vexists 
                 then do 
                   result <- try (writeFile target source  ) :: IO (Either SomeException ())
                   case result of
                      Left ex  -> do 
                        P.putStrLn $ "Caught exception while writing the file " ++ file ++ " - "++ show ex
                        return ((),s)
                      Right val -> do 
                        return ((),updateSel cnode f ([],[Str $ stripNewline source]) s) 

              -- Incorporate into the log file.
              else do
                --vsource <- readFile target
                --let e_vtext = ccParser (T.unpack $ stripNewline vsource)
                --print ("vtext from file: "++ show e_vtext)
                --let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True } 
      
                --errorIf (not v_parsed) $ "Failed to parse " ++ target
                --let Right vtext = e_vtext
                --print vtext
                let (sel,vs) = lookUpSel dag cnode f s--(P.head parents) f s
                let dtext = distill (dim) vs sel{-(latest vtext)-} (stripNewline source)
                writeVFile target ([],dtext) --Exc.catch ( writeFile target $ (T.pack $ showVText dtext)) writeHandler
                {-cont <- readFile target
                I.putStrLn (T.pack (show dim++": "))
                I.putStrLn cont-}
                return $ ((),updateSel cnode f ((RSel dim):sel,dtext) s) 
                
           )   


writeVFile :: FilePath -> (Selection,VString) -> IO ()
writeVFile f (sel, vs) = Exc.catch ( writeFile (f++".v") $ (T.pack $ showVText vs)) writeHandler

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
  P.putStrLn ("Something went wrong during write operation: " ++ show e)   
  return ()

nextDimensionS :: VString -> Int
nextDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (P.maximum ds) + 1

{-stripNewline :: [Char] -> [Char]
stripNewline []                 = []
stripNewline ('\n' :[])         = []
stripNewline (x:xs)             = x : stripNewline xs-}

stripNewline :: Text -> Text
stripNewline s 
  | T.null s          = s
  | T.last s == '\n'  = T.init s
  | otherwise         = s

errorIf :: Bool -> String -> IO ()
errorIf b m = if b then print m else return ()  
           
    
