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
            "--help"  -> printHelp
            "-h"      -> printHelp
            "--build" -> do
                file <- readFile (args !! 1)
                buildFromSource (T.pack file)
            "--lex" -> do 
                file <- readFile (args !! 1)
                let path = args !! 1
                let out = args !! 2
                lexOnly (T.pack file) path out
            xs ->
                putStrLn
                    ("Invalid option \"" ++ xs ++ "\" \n use --help for help")


printHelp :: IO ()
printHelp = putStrLn
    "\n\
\Penguor\n\
\-------\n\n\
\\
\These commands are available:\n\n\
\\
\   --help, -h              : print this help\n\
\   --build <file>          : build a file from source\n\
\           <project>       : not implemented yet\n\
\   --lex <file> <out>      : convert a source file into tokens and serialize the output\n\
\"
