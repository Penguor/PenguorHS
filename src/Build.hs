module Build
    ( buildFromSource
    )
where

import           Data.Text                      ( Text )
import           Text.Megaparsec
import           Parser

buildFromSource :: Text -> IO ()
buildFromSource input = parseTest program input

