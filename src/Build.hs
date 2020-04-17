module Build
    ( buildFromSource
    )
where

import           Data.Text                      ( Text )
import           Text.Megaparsec
import           Parser
import           PLexer

buildFromSource :: Text -> IO ()
buildFromSource input = parseTest tokenize input

