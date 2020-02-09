module Parser
    ( parse
    )
where

import           Parser.TokenType              as TokenType
import           PLexer


programStmt tokens = headStmt tokens

headStmt tokens = consume STRING

parse :: [Token] -> Token
parse (x : xs) = consume AND x

consume :: TokenType -> Token -> Token
consume tokType x 
    | tokType == tType x = x
    | otherwise = error "not equal"