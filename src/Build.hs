{-# LANGUAGE OverloadedStrings #-}

module Build
    ( buildFromSource
    )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Parser
import           PLexer                         ( tokenize )
import           Parser.Token                   ( PStream(..) )

buildFromSource :: Text -> IO ()
buildFromSource input = do
    parseTest tokenize input
    putStr "\n"
    either (putStrLn <$> errorBundlePretty) (parseTest program) (stream <$> toks)
  where
    toks = parse tokenize "" input
    stream a = PStream { streamInput = input, streamTokens = a }

