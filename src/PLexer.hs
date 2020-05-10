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

-- lex identifiers
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

-- build numbers
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

--build a string 
buildString :: Parser Tok
buildString = do
    p1  <- getSourcePos
    str <- between (skipSpace >> single '"')
                   (single '"' <* skipSpace)
                   (many text)
    p2 <- getSourcePos
    return $ Tok STRING (TokenPos p1 p2 (length str)) (T.pack str)
    where text = satisfy (/= '"')

-- ! optimise getOther
getOther :: Parser Tok
getOther = do
    skipSpace
    p1  <- getSourcePos
    cur <- anySingle
    case cur of
        '+' -> do
            t <- next '='
            if t == '=' then newTok ADD_ASSIGN p1 2 else newTok PLUS p1 1
        '-' -> do
            t <- next '='
            if t == '=' then newTok SUB_ASSIGN p1 2 else newTok MINUS p1 1
        '*' -> do
            t <- next '='
            if t == '=' then newTok MUL_ASSIGN p1 2 else newTok MUL p1 1
        '/' -> do
            t <- next '='
            if t == '=' then newTok DIV_ASSIGN p1 2 else newTok DIV p1 1
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
            t <- next '='
            if t == '=' then newTok LESS_EQUALS p1 2 else newTok LESS p1 1
        '>' -> do
            t <- next '='
            if t == '=' then newTok GREATER_EQUALS p1 2 else newTok GREATER p1 1
        '&' -> do
            t <- next '&'
            if t == '&' then newTok AND p1 2 else newTok BW_AND p1 1
        '|' -> do
            t <- next '|'
            if t == '|' then newTok OR p1 2 else newTok BW_OR p1 1
        '^' -> do
            t <- next '^'
            if t == '^' then newTok XOR p1 2 else newTok BW_XOR p1 1
        '#' -> newTok HASHTAG p1 2
        '=' -> do
            t <- next '='
            if t == '=' then newTok EQUALS p1 2 else newTok ASSIGN p1 1

  where
    next s = option ' ' (single s)
    newTok t p1 l = do
        p2 <- getSourcePos
        return $ Tok t (TokenPos p1 p2 l) ""
