module Build
    ( buildFromSource
    )
where

import           Text.Megaparsec
import           Parser

buildFromSource :: String -> String
buildFromSource input = runParser program "" input

