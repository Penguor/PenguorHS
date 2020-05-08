{-# LANGUAGE OverloadedStrings #-}

module Build where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Parser                         ( program )
import           PLexer                         ( tokenize )
import           Parser.Token                   ( PStream(..) )
import           Control.Monad                  ( when )
import           Data.Time.Clock.System
import           Data.Time.Clock

buildFromSource :: Text -> IO ()
buildFromSource input = do
    parseTest tokenize input
    putStr "\n"
    either (putStrLn <$> errorBundlePretty)
           (parseTest program)
           (stream <$> toks)
  where
    toks = parse tokenize "" input
    stream a = PStream { streamInput = input, streamTokens = a }

lexOnly :: Text -> String -> String -> IO ()
lexOnly input file out = do
    st <- getSystemTime
    let toks = parse tokenize file input
    case toks of
        Left  err -> putStrLn (errorBundlePretty err)
        Right t   -> do
            et <- getSystemTime
            putStrLn "Input:"
            putStrLn $ "    File: " ++ file
            putStrLn $ "    Characters: " ++ (show (T.length input))
            putStrLn $ "    Lines: " ++ (show (length (T.lines input)))
            putStrLn $ "Output: "
            putStrLn $ "    File: " ++ out
            putStrLn $ "    Token count: " ++ (show (length t))
            let dt = diffUTCTime (systemToUTCTime et) (systemToUTCTime st) in
                putStrLn $ "\nTime elapsed: " ++ (show dt)
            writeFile out (show t)
