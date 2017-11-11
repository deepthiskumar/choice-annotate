--module GitEncode where

import Prelude as P
import GitDag as G
import CCLibPat
import System.Environment (getArgs)
import Data.Text as T 
import Text.Printf (printf)
import Control.Exception as Exc
import System.Directory (doesFileExist)
import Data.ByteString.Char8 as B hiding (putStrLn, writeFile, readFile)
import Data.Map as M
import Data.Graph.Inductive.Graph
import Control.Monad.Trans.State
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad (foldM)

--TODO
{-
1. get merge base and check if merge has conflicts --Done
 Check sending git executable location. Add a global git config file. Done: Added email and name in the command itself
2. update selections
3. traverse the dag and encode. --Going on: Debug the state values in encode

4. for now merge simply (no conflict => serialize, conflict => branch(even if ours or theirs))
5. then check complex merges
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
    

{-encodeCommits :: FilePath -> GitDag -> CommitNode -> IO ()--(VString,Selection)
encodeCommits repo dag commit = do
  fileList <- runGitCmd repo (gitDiffTree commit)
  mapM (ccEncode repo (fst commit)) (P.map B.unpack fileList)
  mapM (encodeCommits repo dag) (children dag (fst commit))
  return ()
  -}
{-encodeRepo :: FilePath ->  GitDag -> IO ()
encodeRepo repo dag = do
  --get the root commit and start from there
  let (parents,id,commit,children) = commitContext dag 0
  encodeCommits repo (id,commit)-}
  
encodeCommits :: FilePath -> GitDag -> Context Commit () -> SelState ()
encodeCommits repo dag c@(parents,id,commit,children) = do
    fileList <- liftIO $ runGitCmd repo (gitDiffTree (id, commit))
    liftIO $ runGitCmd repo (gitCheckout (B.unpack $ commitID commit))
    mapM (ccEncode dag repo (id,commit) (parent dag id) ) (P.map B.unpack fileList)
    foldM (\_ ctxt -> encodeCommits repo dag ctxt) () (G.children dag id)
   


--foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

{-do
  fileList <- runGitCmd repo (gitDiffTree (id, commit))
  case parents of
    []  -> mapM (ccEncode repo id sel) (P.map B.unpack fileList)
    [x] -> mapM (ccEncode repo id (RSel id : sel)) (P.map B.unpack fileList)
    xs ->mapM (ccEncode repo (fst commit)) (P.map B.unpack (fileList \\ conflictedFiles $ mergeCommit $ commit)) 
    --merge the parents of conflictedFiles and then compare with the merge commit files
  return ()-}
    
--Use state monad to update the map. Seelction needs to be for each file.
    
  

  
ccEncode :: GitDag -> FilePath -> CommitNode -> [CommitNode] -> FilePath -> SelState ()
ccEncode dag repo cnode@(dim,commit) parents f  = do
   StateT $ (\s ->  do
    print (show s) 
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
         source1 <- try (readFile file) :: IO (Either SomeException (String))
         --source <- S.readFile file
         case source1 of
            Left ex -> do 
                 putStrLn $ "Caught exception while reading the source file: " ++ show ex
                 return ((),s)
            Right source -> do
              if not vexists 
                 then do 
                   result <- try (writeFile target source  ) :: IO (Either SomeException ())
                   case result of
                      Left ex  -> do 
                        putStrLn $ "Caught exception while writing the file " ++ file ++ " - "++ show ex
                        return ((),s)
                      Right val -> do 
                        return ((),updateSel cnode f ([],[Str $ T.pack $ stripNewline source]) s) 

              -- Incorporate into the log file.
              else do
                vsource <- readFile target
                let e_vtext = ccParser (stripNewline vsource)
                --print ("vtext from file: "++ show e_vtext)
                let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True } 
      
                errorIf (not v_parsed) $ "Failed to parse " ++ target
                --let Right vtext = e_vtext
                --print vtext
                let (sel,vs) = lookUpSel dag cnode f s--(P.head parents) f s
                --print ("Selection: "++ show sel)
                --print ("vtext from env: "++ show vs)
                let dtext = distill (dim) vs sel{-(latest vtext)-} (T.pack $ stripNewline source)
                Exc.catch ( writeFile target $ showVText dtext) writeHandler
                
                return $ ((),updateSel cnode f ((RSel dim):sel,dtext) s) 
                
           )   

             
lookUpSel :: GitDag -> CommitNode -> FilePath -> RepoInfoMap -> (Selection, VString)
lookUpSel dag cnode f m = 
   case fParent dag (fst cnode) of
     Just p  -> case lookup' p f m of
                  ([],[]) ->lookUpSel dag p f m
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
  putStrLn ("Something went wrong during write operation: " ++ show e)   
  return ()

nextDimensionS :: VString -> Int
nextDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (P.maximum ds) + 1

stripNewline :: [Char] -> [Char]
stripNewline []                 = []
stripNewline ('\n' :[])         = []
stripNewline (x:xs)             = x : stripNewline xs


errorIf :: Bool -> String -> IO ()
errorIf b m = if b then print m else return ()  
           
    
