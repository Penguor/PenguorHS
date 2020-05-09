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
    p1 <- eof *> getSourcePos
    skipSpace
    return (toks ++ getEof p1)
  where
    getEof a = [Tok EOF (TokenPos a a 0) ""]
    sp = initialPos ""

getToken :: Parser Tok
getToken =
    skipSpace >> choice [try buildIdf, try buildNum, try buildString, getOther]

buildIdf :: Parser Tok
buildIdf = do
    skipSpace
    p1    <- getSourcePos
    first <- letterChar <|> char '_'
    rest  <- many (alphaNumChar <|> char '_')
    p2    <- getSourcePos
    skipSpace
    let cp = T.pack (first : rest)
    case cp of
        "fn"        -> return $ newTok FN p1 p2 "fn"
        "null"      -> return $ newTok NULL p1 p2 "null"
        "system"    -> return $ newTok SYSTEM p1 p2 "system"
        "container" -> return $ newTok CONTAINER p1 p2 "container"
        "datatype"  -> return $ newTok DATATYPE p1 p2 "datatype"
        "library"   -> return $ newTok LIBRARY p1 p2 "library"
        "if"        -> return $ newTok IF p1 p2 "if"
        "elif"      -> return $ newTok ELIF p1 p2 "elif"
        "else"      -> return $ newTok ELSE p1 p2 "else"
        "while"     -> return $ newTok WHILE p1 p2 "while"
        "for"       -> return $ newTok FOR p1 p2 "for"
        "do"        -> return $ newTok DO p1 p2 "do"
        "from"      -> return $ newTok FROM p1 p2 "from"
        "include"   -> return $ newTok INCLUDE p1 p2 "include"
        "safety"    -> return $ newTok SAFETY p1 p2 "safety"
        "var"       -> return $ newTok VAR p1 p2 "var"
        "true"      -> return $ newTok TRUE p1 p2 "true"
        "false"     -> return $ newTok FALSE p1 p2 "false"
        "switch"    -> return $ newTok SWITCH p1 p2 "switch"
        "case"      -> return $ newTok CASE p1 p2 "case"
        "default"   -> return $ newTok DEFAULT p1 p2 "default"
        _           -> return $ newIdf p1 p2 cp
  where
    newTok t p1 p2 c = Tok t (TokenPos p1 p2 (T.length c)) ""
    newIdf p1 p2 c = Tok IDF (TokenPos p1 p2 (T.length c)) c

buildNum :: Parser Tok
buildNum = do
    p1    <- getSourcePos
    front <- some digitChar
    dot   <- optional (char '.')
    case dot of
        Nothing -> do
            p2 <- getSourcePos
            return $ Tok NUM (TokenPos p1 p2 (length front)) (T.pack front)
        Just _ -> do
            rest <- some digitChar
            p2   <- getSourcePos
            let cp = T.pack (front ++ "." ++ rest)
            return $ Tok NUM (TokenPos p1 p2 (T.length cp)) cp

buildString :: Parser Tok
buildString = do
    p1  <- getSourcePos
    str <- between (skipSpace >> single '"')
                   (single '"' <* skipSpace)
                   (many text)
    p2 <- getSourcePos
    return $ Tok STRING (TokenPos p1 p2 (length str)) (T.pack str)
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
    , tokBySymbol "+=" EQUALS         ""
    , tokBySymbol "-=" EQUALS         ""
    , tokBySymbol "*=" EQUALS         ""
    , tokBySymbol "/=" EQUALS         ""
    , tokBySymbol "="  ASSIGN         ""
    ]

tokBySymbol :: Text -> TType -> Text -> Parser Tok
tokBySymbol str t vl = do
    p1 <- getSourcePos
    symbol str
    p2 <- getSourcePos
    return $ Tok t (TokenPos p1 p2 (T.length str)) vl
