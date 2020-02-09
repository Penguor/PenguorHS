module Build
    ( buildFromSource
    )
where

import           PLexer
import           Parser

buildFromSource :: String -> IO ()
buildFromSource file = do
    let tokens  = tokenize "fn fn"
    let program = parse tokens
    putStrLn "Build succeeded"
