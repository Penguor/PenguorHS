module Build
    ( buildFromSource
    )
where

import           Text.Megaparsec
import           Parser

buildFromSource :: String -> IO ()
buildFromSource input = parseTest program input

