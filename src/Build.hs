module Build
    ( buildFromSource
    )
where

import           PLexer
import           Parser

buildFromSource :: String -> IO ()
buildFromSource file = do
    let tokens = tokenize file
    putStrLn (show tokens)
