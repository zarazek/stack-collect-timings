module Main where

import Lib
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (encodeDefaultOrderedByName)
-- import Data.Foldable (for_)
import System.Environment (getArgs)

main :: IO ()
main = do
  [dirName, outFileName] <- getArgs
  out <-encodeDefaultOrderedByName <$> readStats dirName
  LBS.writeFile outFileName out 
  
