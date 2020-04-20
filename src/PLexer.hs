{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module PLexer
    ( tokenize
    )
where


import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

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
tokenize = many getToken <* eof

getToken :: Parser Tok
getToken = skipSpace
    >> choice [try buildIdf, try buildNum, try buildString, try getOther]

buildIdf :: Parser Tok
buildIdf = do
    pos <- getSourcePos
    choice
        [ Tok FN pos "" <$ symbol "fn"
        , Tok NULL pos "" <$ symbol "null"
        , Tok SYSTEM pos "" <$ symbol "system"
        , Tok CONTAINER pos "" <$ symbol "container"
        , Tok DATATYPE pos "" <$ symbol "datatype"
        , Tok LIBRARY pos "" <$ symbol "library"
        , Tok IF pos "" <$ symbol "if"
        , Tok ELIF pos "" <$ symbol "elif"
        , Tok ELSE pos "" <$ symbol "else"
        , Tok WHILE pos "" <$ symbol "while"
        , Tok FOR pos "" <$ symbol "for"
        , Tok DO pos "" <$ symbol "do"
        , Tok FROM pos "" <$ symbol "from"
        , Tok INCLUDE pos "" <$ symbol "include"
        , Tok VAR pos "" <$ symbol "var"
        , Tok TRUE pos "" <$ symbol "true"
        , Tok FALSE pos "" <$ symbol "false"
        , Tok SWITCH pos "" <$ symbol "switch"
        , Tok CASE pos "" <$ symbol "case"
        , getIdf pos
        ]

getIdf :: SourcePos -> Parser Tok
getIdf pos = do
    first <- lexeme (letterChar <|> char '_')
    rest  <- many (alphaNumChar <|> char '_')
    return $ Tok IDF pos (T.pack (first : rest))

buildNum :: Parser Tok
buildNum = do
    pos   <- getSourcePos
    front <- some digitChar
    dot   <- optional (char '.')
    case dot of
        Nothing -> return $ Tok NUM pos (T.pack front)
        Just _  -> do
            rest <- some digitChar
            return $ Tok NUM pos (T.pack (front ++ "." ++ rest))

buildString :: Parser Tok
buildString = do
    pos <- getSourcePos
    str <- between (symbol "\"") (symbol "\"") (many anySingle)
    return $ Tok STRING pos (T.pack str)

getOther :: Parser Tok
getOther = do
    pos <- getSourcePos
    choice
        [ Tok PLUS pos "" <$ symbol "+"
        , Tok MINUS pos "" <$ symbol "-"
        , Tok MUL pos "" <$ symbol "*"
        , Tok DIV pos "" <$ symbol "/"
        , Tok LPAREN pos "" <$ symbol "("
        , Tok RPAREN pos "" <$ symbol ")"
        , Tok LBRACE pos "" <$ symbol "{"
        , Tok RBRACE pos "" <$ symbol "}"
        , Tok LBRACK pos "" <$ symbol "["
        , Tok RBRACK pos "" <$ symbol "]"
        , Tok DOT pos "" <$ symbol "."
        , Tok COMMA pos "" <$ symbol ","
        , Tok COLON pos "" <$ symbol ":"
        , Tok SEMICOLON pos "" <$ symbol ";"
        , Tok LESS_EQUALS pos "" <$ symbol "<="
        , Tok GREATER_EQUALS pos "" <$ symbol ">="
        , Tok LESS pos "" <$ symbol "<"
        , Tok GREATER pos "" <$ symbol ">"
        , Tok AND pos "" <$ symbol "&&"
        , Tok OR pos "" <$ symbol "||"
        , Tok HASHTAG pos "" <$ symbol "#"
        , Tok RBRACK pos "" <$ symbol "=="
        , Tok RBRACK pos "" <$ symbol "="
        ]
