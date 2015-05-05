import System.Environment
import System.Directory
import Control.Monad
import Data.List
import Data.Either
import Data.Set (toList, fromList)
import Grammar_v2
import Debug.Trace

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let content_lines = lines contents
                          (grammar, Just wchain) = parseLines content_lines emptyGrammar
                          --conf = createConfiguration grammar wchain
                      putStrLn $ "w chain: " ++ (intercalate " " $ map show wchain)
                      putStrLn $ "Grammar: " ++ show grammar
              else do error "The file doesn't exist!"
