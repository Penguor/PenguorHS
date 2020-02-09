module Main where

import           System.Environment
import           Build
import           PLexer
import           System.IO

main :: IO ()
main = do
    args <- getArgs

    if null args
        then putStrLn "Penguor (c) 2020 Carl Schierig \n\n --help for help"
        else case head args of
            "--build" -> do
                file <- readFile (args !! 1)
                buildFromSource file

