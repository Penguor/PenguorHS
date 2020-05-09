{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module PLexer
    ( tokenize
    )
where


import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.List                     as DL

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Debug

import           Parser.Token


type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace = L.space space1
                    (L.skipLineComment "//" :: Parser ())
                    (L.skipBlockComment "/*" "*/" :: Parser ())

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: Tokens Text -> Parser Text
symbol = L.symbol skipSpace


tokenize :: Parser [Tok]
tokenize = do
    toks <- many getToken
    skipSpace
    pos1 <- eof *> getSourcePos
    skipSpace
    return (toks ++ getEof pos1)
  where
    getEof a = [Tok EOF (TokenPos a a 0) ""]
    sp = initialPos ""

getToken :: Parser Tok
getToken =
    skipSpace >> choice [try buildIdf, try buildNum, try buildString, getOther]

buildIdf :: Parser Tok
buildIdf = do
    skipSpace
    pos1  <- getSourcePos
    first <- alphaNumChar <|> char '_'
    rest  <- many (alphaNumChar <|> char '_')
    pos2  <- getSourcePos
    skipSpace
    let cp = T.pack (first : rest)
    case cp of
        "fn"        -> return $ newTok FN pos1 pos2 "fn"
        "null"      -> return $ newTok NULL pos1 pos2 "null"
        "system"    -> return $ newTok SYSTEM pos1 pos2 "system"
        "container" -> return $ newTok CONTAINER pos1 pos2 "container"
        "datatype"  -> return $ newTok DATATYPE pos1 pos2 "datatype"
        "library"   -> return $ newTok LIBRARY pos1 pos2 "library"
        "if"        -> return $ newTok IF pos1 pos2 "if"
        "elif"      -> return $ newTok ELIF pos1 pos2 "elif"
        "else"      -> return $ newTok ELSE pos1 pos2 "else"
        "while"     -> return $ newTok WHILE pos1 pos2 "while"
        "for"       -> return $ newTok FOR pos1 pos2 "for"
        "do"        -> return $ newTok DO pos1 pos2 "do"
        "from"      -> return $ newTok FROM pos1 pos2 "from"
        "include"   -> return $ newTok INCLUDE pos1 pos2 "include"
        "safety"    -> return $ newTok SAFETY pos1 pos2 "safety"
        "var"       -> return $ newTok VAR pos1 pos2 "var"
        "true"      -> return $ newTok TRUE pos1 pos2 "true"
        "false"     -> return $ newTok FALSE pos1 pos2 "false"
        "switch"    -> return $ newTok SWITCH pos1 pos2 "switch"
        "case"      -> return $ newTok CASE pos1 pos2 "case"
        "default"   -> return $ newTok DEFAULT pos1 pos2 "default"
        _           -> return $ newIdf pos1 pos2 cp
  where
    newTok t p1 p2 c = Tok t (TokenPos p1 p2 (T.length c)) ""
    newIdf p1 p2 c = Tok IDF (TokenPos p1 p2 (T.length c)) c

buildNum :: Parser Tok
buildNum = do
    pos1  <- getSourcePos
    front <- some digitChar
    dot   <- optional (char '.')
    case dot of
        Nothing -> do
            pos2 <- getSourcePos
            return $ Tok NUM (TokenPos pos1 pos2 (length front)) (T.pack front)
        Just _ -> do
            rest <- some digitChar
            pos2 <- getSourcePos
            let cp = T.pack (front ++ "." ++ rest)
            return $ Tok NUM (TokenPos pos1 pos2 (T.length cp)) cp

buildString :: Parser Tok
buildString = do
    pos1 <- getSourcePos
    str  <- between (space >> single '"' <* space) (symbol "\"") (many text)
    pos2 <- getSourcePos
    return $ Tok STRING (TokenPos pos1 pos2 (length str)) (T.pack str)
    where text = satisfy (/= '"')

getOther :: Parser Tok
getOther = choice
    [ tokBySymbol "+"  PLUS           ""
    , tokBySymbol "-"  MINUS          ""
    , tokBySymbol "*"  MUL            ""
    , tokBySymbol "/"  DIV            ""
    , tokBySymbol "!"  EXCL_MARK      ""
    , tokBySymbol "("  LPAREN         ""
    , tokBySymbol ")"  RPAREN         ""
    , tokBySymbol "{"  LBRACE         ""
    , tokBySymbol "}"  RBRACE         ""
    , tokBySymbol "["  LBRACK         ""
    , tokBySymbol "]"  RBRACK         ""
    , tokBySymbol "."  DOT            ""
    , tokBySymbol ","  COMMA          ""
    , tokBySymbol ":"  COLON          ""
    , tokBySymbol ";"  SEMICOLON      ""
    , tokBySymbol "<=" LESS_EQUALS    ""
    , tokBySymbol ">=" GREATER_EQUALS ""
    , tokBySymbol "<"  LESS           ""
    , tokBySymbol ">"  GREATER        ""
    , tokBySymbol "&&" AND            ""
    , tokBySymbol "||" OR             ""
    , tokBySymbol "#"  HASHTAG        ""
    , tokBySymbol "==" EQUALS         ""
    , tokBySymbol "="  ASSIGN         ""
    ]


tokBySymbol :: Text -> TType -> Text -> Parser Tok
tokBySymbol str t vl = do
    pos1 <- getSourcePos
    symbol str
    pos2 <- getSourcePos
    return $ Tok t (TokenPos pos1 pos2 (T.length str)) vl
