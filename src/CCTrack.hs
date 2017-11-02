import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import CCLibPat (distill, latest, ppVText, ccParser, dimensions,showVText, VString)
import Data.List as L
import Control.DeepSeq
import Data.Text as T

errorIf :: Bool -> String -> IO ()
errorIf b m = if b then error m else return ()

nextDimensionS :: VString -> Int
nextDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (L.maximum ds) + 1

stripNewline :: [Char] -> [Char]
stripNewline [] = []
stripNewline ('\n' :[])   = []
stripNewline (x:xs)       = let v = (stripNewline xs) in v `seq` x:v

{- stripNewline :: Text -> Text
stripNewline s 
  | T.null s          = s
  | T.last s == '\n'  = T.init s
  | otherwise         = s -}

main :: IO ()
main = do
  args <- getArgs
  let file = L.head args
  let dimension = read $! (args !! 1) :: Int
  let targetFile = (args !! 2)
  exists <- doesFileExist file
  let target = targetFile ++ ".v"
  vexists <- doesFileExist target
  
  errorIf (L.length args /= 3) "Usage: CCTrack <filename> <dimension> <target filename>"
  errorIf (not exists) $ file ++ " doesnt exist"

  source <- readFile file

  if not vexists 

    -- Create the log file.
    then writeFile target source

    -- Incorporate into the log file.
    else do
      vsource <- readFile target
      --print vsource
      let e_vtext = ccParser $! (stripNewline $ vsource)
      let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True } 
      
      errorIf (not v_parsed) $ "Failed to parse " ++ target
      let Right vtext = e_vtext
      let dtext = (((distill $ (dimension)) $ (vtext)) $ (latest vtext)) $ (T.pack $ stripNewline $ source) --TODO chnage this later 
      
      writeFile target 
         $! showVText dtext

 
