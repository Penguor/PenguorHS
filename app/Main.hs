{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment
import           Build
import           System.IO
import qualified Data.Text                     as T
                                                ( pack )
import           Parser                         ( program )

main :: IO ()
main = do
    args <- getArgs

    if null args
        then putStrLn "Penguor (c) 2020 Carl Schierig \n\n --help for help"
        else case head args of
            "--build" -> do
                file <- readFile (args !! 1)
                buildFromSource (T.pack file)
            xs ->
                putStrLn
                    ("Invalid option \"" ++ xs ++ "\" \n use --help for help")


