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
    pos1 <- eof *> getSourcePos
    return (toks ++ getEof pos1)
  where
    getEof a = [Tok EOF (TokenPos a a 0) ""]
    sp = initialPos ""

getToken :: Parser Tok
getToken =
    skipSpace >> choice [try buildIdf, try buildNum, try buildString, getOther]

buildIdf :: Parser Tok
buildIdf = choice
    [ tokBySymbol "fn"        FN        ""
    , tokBySymbol "null"      NULL      ""
    , tokBySymbol "system"    SYSTEM    ""
    , tokBySymbol "container" CONTAINER ""
    , tokBySymbol "datatype"  DATATYPE  ""
    , tokBySymbol "library"   LIBRARY   ""
    , tokBySymbol "if"        IF        ""
    , tokBySymbol "elif"      ELIF      ""
    , tokBySymbol "else"      ELSE      ""
    , tokBySymbol "while"     WHILE     ""
    , tokBySymbol "for"       FOR       ""
    , tokBySymbol "do"        DO        ""
    , tokBySymbol "from"      FROM      ""
    , tokBySymbol "include"   INCLUDE   ""
    , tokBySymbol "var"       VAR       ""
    , tokBySymbol "true"      TRUE      ""
    , tokBySymbol "false"     FALSE     ""
    , tokBySymbol "switch"    SWITCH    ""
    , tokBySymbol "case"      CASE      ""
    , getIdf
    ]

getIdf :: Parser Tok
getIdf = do
    pos1  <- getSourcePos
    first <- lexeme (letterChar <|> char '_')
    rest  <- many (alphaNumChar <|> char '_')
    pos2  <- getSourcePos
    let cp = T.pack (first : rest)
    return $ Tok IDF (TokenPos pos1 pos2 (T.length cp)) cp

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
