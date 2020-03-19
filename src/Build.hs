module Build
    ( buildFromSource
    )
where

import           Text.Parsec
import           Parser

buildFromSource :: String -> String
buildFromSource input = do
    either (show) (show) (parse program "" input)

