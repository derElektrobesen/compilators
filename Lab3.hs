import System.Environment
import System.Directory
import Control.Monad
import Data.List
import Data.Either
import Data.Set (toList, fromList)
import Grammar

-- main

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let grammar = parseLines $ lines contents
                      putStrLn $ "Default grammar:\n" ++ show grammar
              else do error "The file doesn't exist!"
