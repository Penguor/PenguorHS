module Parser where


import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Char

data Null = Null
    deriving(Show)

data Declaration =
      System String String Block
    | Component String String Block
    | Datatype String String Block
    | Var String String Expression
    | Function String String [(String, String)] Block
    | Library String Block
    | Stmt Statement
    | EmptyDec
    deriving(Show)

data Statement =
      PPStmt PPDirective
    | BlockStmt Block
    | IfStmt
    deriving(Show)

data PPDirective =
      Include String
    | FromIncl String String
    | Safety Int
    deriving(Show)

data Block = Block [Declaration] | EmptyBlock
    deriving(Show)

data Expression = Expression
    deriving(Show)

program :: Parsec String () Declaration
program = declaration

declaration :: Parsec String () Declaration
declaration =
    sysDec <|> compDec <|> dtypeDec <|> varDec <|> functionDec <|> libDec


sysDec :: Parsec String () Declaration
sysDec = do
    try (string "system")
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    System name par <$> blockStmt


compDec :: Parsec String () Declaration
compDec = do
    try (string "component")
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    Component name par <$> blockStmt

dtypeDec :: Parsec String () Declaration
dtypeDec = do
    try (string "datatype")
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    block <- blockStmt
    Datatype name par <$> blockStmt

parent :: Parsec String () (String)
parent = option "" (try $ char '<' >> spaces >> getIdentifier)


varDec :: Parsec String () Declaration
varDec = do
    try (string "var")
    spaces
    typ <- getIdentifier
    spaces
    name <- getIdentifier
    spaces
    expr <- expression
    spaces
    char ';'
    return $ Var typ name expr

functionDec :: Parsec String () Declaration
functionDec = do
    try (string "fn")
    spaces
    typ <- getIdentifier
    spaces
    name <- getIdentifier
    spaces
    params <- between (char '(') (char ')') parameters
    spaces
    Function typ name params <$> blockStmt

libDec :: Parsec String () Declaration
libDec = do
    try (string "library")
    spaces
    name <- getIdentifier
    spaces
    Library name <$> blockStmt







statement :: Parser Declaration
statement = return $ Stmt IfStmt

--preProcessorStmt :: Parser Statement
--preProcessorStmt = do
--    try (char '#')
--    dir <- ppDirective
--    return dir
--
----ppDirective :: Parser String
--ppDirective = (try $ string "dir")

blockStmt :: Parser Block
blockStmt = do
    spaces
    char '{'
    spaces
    dec <- option [EmptyDec] (many declaration)
    spaces
    char '}'
    return $ Block dec


expression :: Parsec String () Expression
expression = return Expression


parameters :: Parsec String () [(String, String)]
parameters = do
    par <- getParam
    try $ char ','
    rest <- option [] parameters
    return $ par : rest

getParam :: Parsec String () (String, String)
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
