
import Prelude hiding (readFile,catch)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import System.Directory
import Control.Monad ( forM, forM_, liftM )
import Debug.Trace ( trace )
import System.FilePath ( (</>) )
import Data.List
import System.Process
import GHC.IO.Handle
import Data.Maybe
import CCLibPat (distill, latest, ppVText, ccParser, dimensions,showVText)
import VText hiding (Text)
import qualified Data.Map.Strict as Map
import Control.Exception as Exc
import System.IO.Error
--import qualified Data.ByteString.Strict as BSL
import System.IO.Unsafe
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

type Commit = String
type VTextMap = Map.Map FilePath VText

ignoreDirectories  = [".git"]
ignoreFiles = [".class", "gestures",  ".svg", ".m4a",".mp4",".mp3",".bin",".gitignore",".png",".jar",".gitattributes",".v",".jks",".apk",".ttf",".jpg",".git",  ".DS_Store", ".jpeg"]

--Repositories are named as repo1, repo2 and so on. x and y are the bounds of these number suffixes
--you can pass it as arguments but this is 
--Usage: ./CCEncode ReposDIr StartRepoNumber EndRepoNumber
main :: IO ()
main = do
  args <- getArgs
  let reposDir = head args
  let startRepo = read $ (head.tail) $ args :: Int
  let endRepo = read $ (head.tail.tail) $ args :: Int
  encodeAllRepo reposDir startRepo endRepo --only 1 repo

--File path is the directory that contains all the repositories that you want to encode
encodeAllRepo :: FilePath -> Int -> Int -> IO()
encodeAllRepo f x y
  | x > y    = return ()
  | otherwise = do 
         start <- getCPUTime
         startEncoding (f++"/repo" ++ show x)
         end   <- getCPUTime
         let diff = (fromIntegral (end - start)) / (10^12)
         printf "Computation time for repo %d: %0.3f sec\n" x (diff :: Double)
         encodeAllRepo f (x + 1) y

startEncoding :: FilePath -> IO()
startEncoding repo = do
  print ("started encoding " ++ repo)
  commits <- getAllCommits repo
  print $ "Total commits : " ++ show (length commits) 
  encodeFile repo commits
  print "Done"

getAllCommits :: FilePath -> IO [Commit]
getAllCommits repo = do
  let process = proc "git" ["log","--pretty=format:%H", "--reverse"]--, "--no-merges"]
  (_,Just h,e,_) <- createProcess  process { cwd = Just repo, std_out = CreatePipe}
  if (isJust e) 
    then do
     print ("Error while running command : git log --pretty=format:%H --reverse in " ++ repo ++". Error :" ++ show e)
     return []
     else do
      commits <- hGetContents h
      let commitList = (lines commits)
      print "commit list"
      print commitList
      return commitList   


encodeFile :: FilePath -> [Commit] -> IO ()
encodeFile _ [] = return ()
encodeFile repo (x:xs) = do
  encodeRootCommit repo x --seperate command for root, hence dealing with it seperately
  loopEncoding repo xs 1
  return ()

loopEncoding :: FilePath -> [Commit] ->Int -> IO ()
loopEncoding _ [] _ = return ()
loopEncoding repo (x:xs) dim = do
  encodeOtherCommits repo x dim
  loopEncoding repo xs (dim + 1)

encodeOtherCommits :: FilePath -> String ->Int -> IO ()
encodeOtherCommits repo commit dim= do
  print ("Commit : " ++ commit ++ " Dimension :" ++ show dim )
  files <- getCommitFiles repo commit
  print "file list"
  print files
  checkout repo commit
  mapM (ccTrack repo dim) (removeBinaryFiles files)
  return ()
  
removeBinaryFiles :: [FilePath] -> [FilePath]
removeBinaryFiles []     = []
removeBinaryFiles (x:xs) = case not(or (map (`isInfixOf` x) ignoreFiles)) of
          True -> x : removeBinaryFiles xs
          False -> removeBinaryFiles xs

encodeRootCommit :: FilePath -> Commit -> IO ()
encodeRootCommit repo commit = do
  let process = proc "git" ["diff-tree","--no-commit-id", "--name-only", "-r", "--root", commit]
  (_,Just h,e,_) <- createProcess  process { cwd = Just repo, std_out = CreatePipe}
  if (isJust e)
  then do
    print ("Error while running command : git diff-tree --no-commit-id --name-only -r --root " ++ commit ++ ". Error :" ++ show e)
    return ()
    else do
      files <- hGetContents h
      let fileList = (lines files)
      print "file list"
      print fileList
      checkout repo commit
      mapM (ccTrack repo 0) (removeBinaryFiles fileList)
      return ()

getCommitFiles :: FilePath -> Commit -> IO [FilePath]
getCommitFiles repo commit = do 
  let process = proc "git" ["diff-tree","--no-commit-id", "--name-only", "-r", commit]
  (_,Just h,e,_) <- createProcess  process { cwd = Just repo, std_out = CreatePipe}
  if (isJust e) then do
   print ("Error while running command : git diff-tree --no-commit-id --name-only -r " ++ commit ++ ". Error :" ++ show e)
   return []
     else do
      files <- hGetContents h
      let fileList = (lines files)
      print "file list"
      return fileList

checkout :: FilePath -> Commit -> IO()
checkout repo commit = do
  print ("started encoding commit " ++ commit)
  (_,s,e,_) <- createProcess (proc "git" ["checkout",commit]) { cwd = Just repo, std_out = CreatePipe}
  if(isJust e)
    then print ("Error while running command : git checkout " ++ commit)
      else do
        if(isJust s)
          then do
           let Just h = s
           val <- hGetContents h
           print ("val: " ++ show val)
          else do print "Error in handle"

ccTrack :: FilePath -> Int -> FilePath -> IO ()
ccTrack repo dim f  = do
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
                let dtext = distill (dim) vtext (latest vtext) (stripNewline source)
                Exc.catch ( writeFile target $ showVText dtext) writeHandler
                return ()

writeHandler :: IOError -> IO ()
writeHandler e = do 
  putStrLn "Something went wrong during write operation"  
  return ()

nextDimensionS :: VText -> Int
nextDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (maximum ds) + 1

stripNewline :: [Char] -> [Char]
stripNewline []                 = []
stripNewline ('\n' :[])         = []
stripNewline (x:xs)             = x : stripNewline xs


errorIf :: Bool -> String -> IO ()
errorIf b m = if b then print m else return ()
