module Main where

import           System.Environment
import           Build
import           PLexer
import           System.IO

main :: IO ()
main = do
    args <- getArgs

    case head args of
        "--build" -> do
            file <- readFile (args !! 1)
            buildFromSource file

