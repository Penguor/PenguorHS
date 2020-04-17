{-# LANGUAGE OverloadedStrings #-}

module PLexer
    ( tokenize
    , Token(..)
    )
where


--import           Data.Char

import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Text.Megaparsec         hiding ( Token )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import qualified Parser.TokenType              as TokenType


data Token = Token { token :: Text, tType :: TokenType.TokenType, pos :: SourcePos}
    deriving(Show, Eq)

type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace = L.space space1
                    (L.skipLineComment "//" :: Parser ())
                    (L.skipBlockComment "/*" "*/" :: Parser ())

lexeme = L.lexeme skipSpace
symbol = L.symbol skipSpace


tokenize :: Parser [Token]
tokenize = many getToken <* eof

getToken :: Parser Token
getToken = do
    pos <- getSourcePos
    choice [buildIdf, buildNum, buildString, getOther]

buildIdf :: Parser Token
buildIdf = do
    pos <- getSourcePos
    choice
        [ Token "fn" TokenType.HASHTAG pos <$ symbol "fn"
        , Token "null" TokenType.HASHTAG pos <$ symbol "null"
        , Token "system" TokenType.HASHTAG pos <$ symbol "system"
        , Token "component" TokenType.HASHTAG pos <$ symbol "component"
        , Token "datatype" TokenType.HASHTAG pos <$ symbol "datatype"
        , Token "if" TokenType.HASHTAG pos <$ symbol "if"
        , Token "while" TokenType.HASHTAG pos <$ symbol "while"
        , Token "for" TokenType.HASHTAG pos <$ symbol "for"
        , Token "do" TokenType.HASHTAG pos <$ symbol "do"
        , Token "from" TokenType.HASHTAG pos <$ symbol "from"
        , Token "include" TokenType.HASHTAG pos <$ symbol "include"
        , Token "var" TokenType.HASHTAG pos <$ symbol "var"
        , Token "true" TokenType.HASHTAG pos <$ symbol "true"
        , Token "false" TokenType.HASHTAG pos <$ symbol "false"
        , Token "switch" TokenType.HASHTAG pos <$ symbol "switch"
        , Token "case" TokenType.HASHTAG pos <$ symbol "case"
        , getIdf pos
        ]

getIdf :: SourcePos -> Parser Token
getIdf pos = do
    first <- lexeme (letterChar <|> char '_')
    rest  <- many (alphaNumChar <|> char '_')
    return $ Token (T.pack (first : rest)) TokenType.IDF pos

buildNum :: Parser Token
buildNum = do
    pos   <- getSourcePos
    front <- some digitChar
    dot   <- optional (char '.')
    case dot of
        Nothing -> return $ Token (T.pack front) TokenType.NUM pos
        Just _  -> do
            rest <- some digitChar
            return $ Token (T.pack (front ++ "." ++ rest)) TokenType.NUM pos

buildString :: Parser Token
buildString = do
    pos <- getSourcePos
    str <- between (symbol "\"") (symbol "\"") (many anySingle)
    return $ Token (T.pack str) TokenType.STRING pos

getOther :: Parser Token
getOther = do
    pos <- getSourcePos
    choice [Token "+" TokenType.PLUS pos <$ symbol "+"]
