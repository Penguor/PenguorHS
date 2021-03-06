{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module PLexer
    ( tokenize
    )
where


import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Char

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

-- lex identifiers
buildIdf :: Parser Tok
buildIdf = do
    skipSpace
    p1    <- getSourcePos
    first <- takeWhile1P (Just "letter or underscore") fst
    rest  <- takeWhileP (Just "letter, digit, or underscore") other
    p2    <- getSourcePos
    skipSpace
    let cp = T.append first rest
    case cp of
        "null"       -> return $ newTok NULL p1 p2 "null"
        "system"     -> return $ newTok SYSTEM p1 p2 "system"
        "container"  -> return $ newTok CONTAINER p1 p2 "container"
        "datatype"   -> return $ newTok DATATYPE p1 p2 "datatype"
        "library"    -> return $ newTok LIBRARY p1 p2 "library"
        "if"         -> return $ newTok IF p1 p2 "if"
        "elif"       -> return $ newTok ELIF p1 p2 "elif"
        "else"       -> return $ newTok ELSE p1 p2 "else"
        "while"      -> return $ newTok WHILE p1 p2 "while"
        "for"        -> return $ newTok FOR p1 p2 "for"
        "do"         -> return $ newTok DO p1 p2 "do"
        "from"       -> return $ newTok FROM p1 p2 "from"
        "include"    -> return $ newTok INCLUDE p1 p2 "include"
        "safety"     -> return $ newTok SAFETY p1 p2 "safety"
        "public"     -> return $ newTok PUBLIC p1 p2 "public"
        "private"    -> return $ newTok PRIVATE p1 p2 "private"
        "protected"  -> return $ newTok PROTECTED p1 p2 "protected"
        "restricted" -> return $ newTok RESTRICTED p1 p2 "restricted"
        "static"     -> return $ newTok STATIC p1 p2 "static"
        "dynamic"    -> return $ newTok DYNAMIC p1 p2 "dynamic"
        "abstract"   -> return $ newTok ABSTRACT p1 p2 "abstract"
        "const"      -> return $ newTok CONST p1 p2 "const"
        "true"       -> return $ newTok TRUE p1 p2 "true"
        "false"      -> return $ newTok FALSE p1 p2 "false"
        "switch"     -> return $ newTok SWITCH p1 p2 "switch"
        "case"       -> return $ newTok CASE p1 p2 "case"
        "default"    -> return $ newTok DEFAULT p1 p2 "default"
        _            -> return $ newIdf p1 p2 cp
  where
    newTok t p1 p2 c = Tok t (TokenPos p1 p2 (T.length c)) ""
    newIdf p1 p2 c = Tok IDF (TokenPos p1 p2 (T.length c)) c
    fst a = isLetter a || (a == '_')
    other a = isAlphaNum a || (a == '_')

-- build numbers
buildNum :: Parser Tok
buildNum = do
    p1    <- getSourcePos
    front <- takeWhile1P (Just "digit") isDigit
    dot   <- optional (char '.')
    case dot of
        Nothing -> do
            p2 <- getSourcePos
            return $ Tok NUM (TokenPos p1 p2 (T.length front)) front
        Just _ -> do
            rest <- takeWhile1P (Just "digit") isDigit
            p2   <- getSourcePos
            let cp = T.append front (T.append "." rest)
            return $ Tok NUM (TokenPos p1 p2 (T.length cp)) cp

--build a string 
buildString :: Parser Tok
buildString = do
    skipSpace
    p1  <- getSourcePos
    str <- between (single '"')
                   (single '"')
                   (takeWhileP (Just "any char but '\"'") text)
    p2 <- getSourcePos
    skipSpace
    return $ Tok STRING (TokenPos p1 p2 (T.length str)) str
    where text a = a /= '"'


getOther :: Parser Tok
getOther = do
    skipSpace
    p1  <- getSourcePos
    cur <- anySingle
    case cur of
        '+' -> do
            t <- mulNext "+="
            case t of
                '+' -> newTok DPLUS p1 2
                '=' -> newTok ADD_ASSIGN p1 2
                _   -> newTok PLUS p1 1
        '-' -> do
            t <- mulNext "-="
            case t of
                '-' -> newTok DMINUS p1 2
                '=' -> newTok SUB_ASSIGN p1 2
                _   -> newTok MINUS p1 1
        '*' -> do
            t <- next '='
            if t then newTok MUL_ASSIGN p1 2 else newTok MUL p1 1
        '/' -> do
            t <- next '='
            if t then newTok DIV_ASSIGN p1 2 else newTok DIV p1 1
        '%' -> do
            t <- next '='
            if t then newTok PERCENT_ASSIGN p1 2 else newTok PERCENT p1 1
        '!' -> newTok EXCL_MARK p1 1
        '~' -> newTok BW_NOT p1 1
        '(' -> newTok LPAREN p1 1
        ')' -> newTok RPAREN p1 1
        '{' -> newTok LBRACE p1 1
        '}' -> newTok RBRACE p1 1
        '[' -> newTok LBRACK p1 1
        ']' -> newTok RBRACK p1 1
        '.' -> newTok DOT p1 1
        ',' -> newTok COMMA p1 1
        ':' -> newTok COLON p1 1
        ';' -> newTok SEMICOLON p1 1
        '<' -> do
            t <- mulNext "<="
            case t of
                '<' -> do
                    a <- next '='
                    if a
                        then newTok BS_LEFT_ASSIGN p1 3
                        else newTok BS_LEFT p1 2
                '=' -> newTok LESS_EQUALS p1 2
                _   -> newTok LESS p1 1
        '>' -> do
            t <- mulNext ">="
            case t of
                '>' -> do
                    a <- next '='
                    if a
                        then newTok BS_LEFT_ASSIGN p1 3
                        else newTok BS_LEFT p1 2
                '=' -> newTok GREATER_EQUALS p1 2
                _   -> newTok GREATER p1 1
        '&' -> do
            t <- mulNext "&="
            case t of
                '&' -> newTok AND p1 2
                '=' -> newTok BW_AND_ASSIGN p1 2
                _   -> newTok BW_AND p1 1
        '|' -> do
            t <- mulNext "|="
            case t of
                '|' -> newTok OR p1 2
                '=' -> newTok BW_OR_ASSIGN p1 2
                _   -> newTok BW_OR p1 1
        '^' -> do
            t <- mulNext "^="
            case t of
                '^' -> newTok XOR p1 2
                '=' -> newTok BW_XOR_ASSIGN p1 2
                _   -> newTok BW_XOR p1 1
        '#' -> newTok HASHTAG p1 2
        '=' -> do
            t <- next '='
            if t then newTok EQUALS p1 2 else newTok ASSIGN p1 1

  where
    next s = do
        t <- optional (single s)
        case t of
            Just _  -> return True
            Nothing -> return False
    newTok t p1 l = do
        p2 <- getSourcePos
        return $ Tok t (TokenPos p1 p2 l) ""

mulNext :: String -> Parser Char
mulNext a = option ' ' $ oneOf a
