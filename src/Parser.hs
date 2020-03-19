module Parser where


import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Char


data Declaration =
      System String String Block
    | Component String String Block
    | Datatype String String Block
    | Var String String Expression
    | Function String String [(String, String)] Block
    | Library String Block
    | Stmt Statement
    deriving(Show)

data Statement =
      PPStmt PPDirective
    | BlockStmt Block
    | IfStmt
    deriving(Show)

data PPDirective =
      Include String
    | FromIncl String String
    | Safety Integer
    deriving(Show)

data Block = Block [Declaration] | EmptyBlock
    deriving(Show)

data Expression = Expression
    deriving(Show)

program :: Parser [Declaration]
program = many1 declaration

declaration :: Parser Declaration
declaration =
    try sysDec
        <|> try compDec
        <|> try dtypeDec
        <|> try varDec
        <|> try functionDec
        <|> try libDec
        <|> try statement
        <?> "error on parsing declaration"


sysDec :: Parser Declaration
sysDec = do
    string "system" <?> "expected 'system'"
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    System name par <$> blockStmt


compDec :: Parser Declaration
compDec = do
    string "component"
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    Component name par <$> blockStmt

dtypeDec :: Parser Declaration
dtypeDec = do
    string "datatype"
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    block <- blockStmt
    Datatype name par <$> blockStmt

parent :: Parser (String)
parent = option "" (try $ char '<' >> spaces >> getIdentifier)


varDec :: Parser Declaration
varDec = do
    string "var"
    spaces
    typ <- getIdentifier
    spaces
    name <- getIdentifier
    spaces
    expr <- expression
    spaces
    char ';'
    return $ Var typ name expr

functionDec :: Parser Declaration
functionDec = do
    string "fn"
    spaces
    typ <- getIdentifier
    spaces
    name <- getIdentifier
    spaces
    params <- between (char '(') (char ')') parameters
    spaces
    Function typ name params <$> blockStmt

libDec :: Parser Declaration
libDec = do
    string "library"
    spaces
    name <- getIdentifier
    spaces
    Library name <$> blockStmt


statement :: Parser Declaration
statement = (Stmt <$> preProcessorStmt)

preProcessorStmt :: Parser Statement
preProcessorStmt = try (char '#') >> PPStmt <$> ppDirective

ppDirective :: Parser PPDirective
ppDirective = include <|> fromIncl <|> safety

include :: Parser PPDirective
include = try (string "include ") >> Include <$> getIdentifier

fromIncl :: Parser PPDirective
fromIncl = do
    try (string "from ")
    lib <- getIdentifier
    string "include "
    FromIncl lib <$> getIdentifier

safety :: Parser PPDirective
safety = do
    try (string "safety ")
    level <- oneOf "012"
    return $ Safety $ read [level]


blockStmt :: Parser Block
blockStmt = do
    spaces
    char '{' <?> "Expected '{'"
    spaces
    dec <- manyTill declaration (char '}') <?> "error in BlockStmt"
    spaces
    return $ Block dec


expression :: Parser Expression
expression = return Expression


parameters :: Parser [(String, String)]
parameters = do
    par <- getParam
    try $ char ','
    rest <- option [] parameters
    return $ par : rest

getParam :: Parser (String, String)
getParam = do
    spaces
    typ <- getIdentifier
    spaces
    name <- getIdentifier
    spaces
    return (typ, name)


getIdentifier :: Parser String
getIdentifier = many1 letter <> many alphaNum

getString :: Parser String
getString = do
    satisfy (== '"')
    str <- many text
    satisfy (== '"')
    return str
    where text = satisfy (/= '"')
