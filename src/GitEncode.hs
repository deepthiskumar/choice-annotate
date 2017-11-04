module GitEncode where

import Prelude as P
import GitDag
import CCLibPat
import System.Environment (getArgs)
import Data.Text as T 
import Text.Printf (printf)
import Control.Exception as Exc
import System.Directory (doesFileExist)
import Data.ByteString.Lazy.Char8 as B hiding (putStrLn, writeFile, readFile)
import Data.Map


type RepoInfo = Map CommitNode [(FilePath, Selection)]

main :: IO()
main = do
    args <- getArgs
    let repo = P.head args
    let branch = ((P.head).(P.tail)) args
    --create a dag from the commits
    dag <- getDag repo branch
    --now for each commit, encode all the files in it
    let (parents,id,commit,children) = commitContext dag 0
    encodeCommits repo dag (id,commit)
    

encodeCommits :: FilePath -> GitDag -> CommitNode -> IO ()--(VString,Selection)
encodeCommits repo dag commit = do
  fileList <- runGitCmd repo (gitDiffTree commit)
  mapM (ccEncode repo (fst commit)) (P.map B.unpack fileList)
  mapM (encodeCommits repo dag) (children dag (fst commit))
  return ()
  
{-encodeRepo :: FilePath ->  GitDag -> IO ()
encodeRepo repo dag = do
  --get the root commit and start from there
  let (parents,id,commit,children) = commitContext dag 0
  encodeCommits repo (id,commit)-}
  

  
ccEncode :: FilePath -> Int -> FilePath -> IO ()
ccEncode repo dim f  = do
    let file = (repo ++ "/" ++ f)
    print ("In ccTrack block :" ++ file)
    exists <- doesFileExist file
    let target = file ++ ".v"
    vexists <- doesFileExist target
    if(not exists)
    then do
         print $ "Source " ++ file ++ " doesnt exist"
         return ()
    else do
         source1 <- try (readFile file) :: IO (Either SomeException (String))
         --source <- S.readFile file
         case source1 of
            Left ex -> do 
                 putStrLn $ "Caught exception while reading the source file: " ++ show ex
                 return ()
            Right source -> do
              if not vexists 
                 then do 
                   result <- try (writeFile target source  ) :: IO (Either SomeException ())
                   case result of
                      Left ex  -> do 
                        putStrLn $ "Caught exception while encoding the file " ++ file ++ " - "++ show ex
                        return ()
                      Right val -> do 
                        return () 

              -- Incorporate into the log file.
              else do
                vsource <- readFile target
                let e_vtext = ccParser (stripNewline vsource)
                let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True } 
      
                errorIf (not v_parsed) $ "Failed to parse " ++ target
                let Right vtext = e_vtext
                let dtext = distill (dim) vtext (latest vtext) (T.pack $ stripNewline source)
                Exc.catch ( writeFile target $ showVText dtext) writeHandler
                return () 
             
writeHandler :: IOError -> IO ()
writeHandler e = do 
  putStrLn "Something went wrong during write operation"  
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
    
