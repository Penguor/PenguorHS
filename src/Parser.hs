module Parser where


import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Char

data Null = Null
    deriving(Show)

data Declaration = System String String Block | Component String String Block | Datatype String String Block | EmptyDec
    deriving(Show)

data Block = Block [Declaration] | EmptyBlock
    deriving(Show)

program :: Parsec String () Declaration
program = declaration

declaration :: Parsec String () Declaration
declaration = sysDec <|> compDec <|> dtypeDec


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


--libDec :: Parser String
--libDec = do
--    try (string "library")
--    spaces
--    name <- getIdentifier
--    spaces
--    parent <- try (char '<') *> spaces *> getIdentifier
--    spaces
--    block <- blockStmt
--    return (name parent block)







----statement :: Parser Statement
--statement = preProcessorStmt <|> blockStmt
--
-----preProcessorStmt :: Parser Statement
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




getIdentifier :: Parser String
getIdentifier = many1 letter <> many alphaNum

getString :: Parser String
getString = do
    satisfy (== '"')
    str <- many text
    satisfy (== '"')
    return str
    where text = satisfy (/= '"')
