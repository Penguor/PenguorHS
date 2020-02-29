module Build
    ( buildFromSource
    )
where

import           PLexer
import           Parser

buildFromSource :: String -> IO ()
buildFromSource file = do
    let tokens  = tokenize file
    let program = parse tokens
    putStrLn (show tokens)
