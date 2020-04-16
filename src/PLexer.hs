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
tokenize (x : xs)
    | x == ' ' = tokenize xs
    | isAlpha x = buildIdf (x : xs)
    | isDigit x = buildNum (x : xs)
    | x == '"' = buildString xs
    | -- todo: line position counter
      isNewLine x = tokenize xs
    | x == '/' = buildComment xs
    | x == '#' = Token "#" TokenType.HASHTAG : buildIdf xs
    | x == '&' = if head xs == '&'
        then Token "&&" TokenType.AND : tokenize (tail xs)
        else do
            error "missing &"
            [Token "" TokenType.OTHER]
    | x == '|' = if head xs == '|'
        then Token "&&" TokenType.OR : tokenize (tail xs)
        else do
            error "missing |"
            [Token "" TokenType.OTHER]
    | x == '=' = if head xs == '='
        then Token "==" TokenType.EQUALS : tokenize (tail xs)
        else Token "=" TokenType.ASSIGN : tokenize xs
    | x == '+' = Token "+" TokenType.PLUS : tokenize xs
    | x == '-' = Token "-" TokenType.MINUS : tokenize xs
    | x == '*' = Token "*" TokenType.MUL : tokenize xs
    | x == '(' = Token "(" TokenType.LPAREN : tokenize xs
    | x == ')' = Token ")" TokenType.RPAREN : tokenize xs
    | x == '{' = Token "[" TokenType.LBRACE : tokenize xs
    | x == '}' = Token "[" TokenType.RBRACE : tokenize xs
    | x == '[' = Token "[" TokenType.LBRACK : tokenize xs
    | x == ']' = Token "[" TokenType.RBRACK : tokenize xs
    | x == ':' = Token "[" TokenType.COLON : tokenize xs
    | x == ';' = Token "[" TokenType.SEMICOLON : tokenize xs
    | x == '<' = Token "[" TokenType.LESS : tokenize xs
    | x == '>' = Token "[" TokenType.GREATER : tokenize xs



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
    | idf == "switch"    = Token "switch" TokenType.SWITCH : tokenize rest
    | idf == "case"      = Token "case" TokenType.CASE : tokenize rest
    | otherwise          = Token idf TokenType.IDF : tokenize rest
    where (idf, rest) = span isAlphaNum xs


buildNum :: String -> [Token]
buildNum xs = Token num TokenType.NUM : tokenize rest
    where (num, rest) = span isNumChr xs

buildString :: String -> [Token]
buildString xs = Token str TokenType.STRING : tokenize (drop 1 rest)
    where (str, rest) = span (/= quote) xs

buildComment :: String -> [Token]
buildComment (x : xs)
    | -- todo: debug line comment
      x == '/' = tokenize (dropWhile isNewLine xs)
    | x == '*' = if head xs == '/'
        then tokenize (tail xs)
        else buildComment (dropWhile (/= '*') xs)


quote :: Char
quote = '"'

isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine '\r' = True
isNewLine _    = False

isNumChr :: Char -> Bool
isNumChr x | x `elem` ['0' .. '9'] = True
           | x == '.'              = True
           | otherwise             = False
