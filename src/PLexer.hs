module PLexer
    ( tokenize
    , Token(..)
    )
where


import           Data.Char
import qualified Parser.TokenType              as TokenType


data Token = Token { token :: String, tType :: TokenType.TokenType}
    deriving(Show, Eq)



tokenize :: String -> [Token]
tokenize "" = [Token "" TokenType.EOF]
tokenize (x : xs) | x == ' '  = tokenize xs
                  | isAlpha x = buildIdf (x : xs)


buildIdf :: String -> [Token]
buildIdf xs
    | idf == "fn"        = Token "fn" TokenType.FN : tokenize rest
    | idf == "null"      = Token "null" TokenType.NULL : tokenize rest
    | idf == "system"    = Token "system" TokenType.SYSTEM : tokenize rest
    | idf == "component" = Token "component" TokenType.COMPONENT : tokenize rest
    | idf == "datatype"  = Token "datatype" TokenType.DATATYPE : tokenize rest
    | idf == "if"        = Token "if" TokenType.IF : tokenize rest
    | idf == "while"     = Token "while" TokenType.WHILE : tokenize rest
    | idf == "for"       = Token "for" TokenType.FOR : tokenize rest
    | idf == "do"        = Token "do" TokenType.DO : tokenize rest
    | idf == "from"      = Token "from" TokenType.FROM : tokenize rest
    | idf == "include"   = Token "include" TokenType.INCLUDE : tokenize rest
    | idf == "var"       = Token "var" TokenType.VAR : tokenize rest
    | idf == "true"      = Token "true" TokenType.TRUE : tokenize rest
    | idf == "false"     = Token "false" TokenType.FALSE : tokenize rest
    | otherwise          = Token idf TokenType.IDF : tokenize rest
    where (idf, rest) = span isAlphaNum xs

