module Build
    ( buildFromSource
    )
where

import           Data.Text                      ( Text )
import           Text.Megaparsec
import           Parser
import           PLexer                         ( tokenize )

buildFromSource :: Text -> IO ()
buildFromSource input =
    either (fail "unknown error") (parseTest program) (parse tokenize "" input)


