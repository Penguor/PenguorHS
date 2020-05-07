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

buildFromSource :: (Show a) => Parser a -> Text -> IO ()
buildFromSource p input = do
    parseTest tokenize input
    putStr "\n"
    either (putStrLn <$> errorBundlePretty) (parseTest p) (stream <$> toks)
  where
    toks = parse tokenize "" input
    stream a = PStream { streamInput = input, streamTokens = a }

