{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Data.Maybe
import           Data.Data
import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Read

import           Text.Megaparsec         hiding ( single )
import qualified Text.Megaparsec               as MP
                                                ( single )

import           Parser.Token
import           PLexer



type Parser = Parsec Void [Tok]

data Declaration =
      System Text Text Block
    | Container Text Text Block
    | Datatype Text Text Block
    | Var Text Text
    | Function Text Text [(Text, Text)] Block
    | Library Text Block
    | Stmt Statement
    deriving(Show, Eq)

data Statement =
      PPStmt PPDirective
    | BlockStmt Block
    | IfStmt Expression [Statement] [Elif] [Statement]
    | WhileStmt Expression [Statement]
    | ForStmt (Text, Text) Expression [Statement]
    | DoStmt [Statement] Expression
    | SwitchStmt Text [Statement] (Maybe [Statement])
    | CaseStmt Expression [Statement]
    | ExprStmt Expression
    deriving(Show, Eq)

data PPDirective =
      Include Text
    | FromIncl Text Text
    | Safety Text
    deriving(Show, Eq)

data Block = Block [Declaration] | EmptyBlock
    deriving(Show, Eq)

data Elif = Elif Expression [Statement]
    deriving(Show, Eq)

data Expression =
      AssignExpr Expression Expression
    | BinaryExpr Expression TType Expression
    | BooleanExpr Bool
    | CallExpr [Text] [Expression]
    | GroupingExpr Expression
    | IdentifierExpr Text
    | NullExpr
    | NumExpr Double
    | StringExpr Text
    | UnaryExpr TType Expression
    | BaseExpr Tok
    deriving(Show, Eq)


program :: Parser [Declaration]
program = some declaration

declaration :: Parser Declaration
declaration = choice
    [sysDec, contDec, dtypeDec, varDec, functionDec, libDec, Stmt <$> statement]


sysDec :: Parser Declaration
sysDec = do
    getByType SYSTEM
    name <- getIdentifier
    par  <- parent
    System name par <$> blockStmt


contDec :: Parser Declaration
contDec = do
    getByType CONTAINER
    name <- getIdentifier
    par  <- parent
    Container name par <$> blockStmt

dtypeDec :: Parser Declaration
dtypeDec = do
    getByType DATATYPE
    name <- getIdentifier
    par  <- parent
    Datatype name par <$> blockStmt

parent :: Parser Text
parent = option "" (try $ getByType LESS >> getIdentifier)


varDec :: Parser Declaration
varDec = do
    getByType VAR
    typ  <- getIdentifier
    name <- getIdentifier <* getByType SEMICOLON
    return $ Var typ name

functionDec :: Parser Declaration
functionDec = do
    getByType FN
    typ    <- getIdentifier
    name   <- getIdentifier
    params <- between (getByType LPAREN) (getByType RPAREN) $ option [] $ try
        parameters

    Function typ name params <$> blockStmt

libDec :: Parser Declaration
libDec = do
    getByType LIBRARY
    name <- getIdentifier
    Library name <$> blockStmt


statement :: Parser Statement
statement = choice
    [preProcessorStmt, ifStmt, whileStmt, forStmt, doStmt, switchStmt, exprStmt]


preProcessorStmt :: Parser Statement
preProcessorStmt = getByType HASHTAG >> PPStmt <$> ppDirective

ppDirective :: Parser PPDirective
ppDirective = try include <|> try fromIncl <|> try safety

include :: Parser PPDirective
include = getByType INCLUDE >> Include <$> getIdentifier

fromIncl :: Parser PPDirective
fromIncl = do
    getByType FROM
    lib <- getIdentifier
    getByType INCLUDE
    FromIncl lib <$> getIdentifier

safety :: Parser PPDirective
safety = do
    getByType SAFETY
    level <- choice [content "1", content "2", content "3"]
    return $ Safety (txt level)


blockStmt :: Parser Block
blockStmt =
    Block <$> between (getByType LBRACE) (getByType RBRACE) (many declaration)

ifStmt :: Parser Statement
ifStmt = do
    getByType IF
    condition  <- between (getByType LPAREN) (getByType RPAREN) expression
    code       <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    elifBlocks <- many elif
    els        <- optional $ getByType ELSE
    case els of
        Nothing -> return $ IfStmt condition code elifBlocks []
        Just x  -> IfStmt condition code elifBlocks <$> some statement


elif :: Parser Elif
elif = do
    getByType ELIF
    condition <- between (getByType LPAREN) (getByType RPAREN) expression
    code      <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    return $ Elif condition code


whileStmt :: Parser Statement
whileStmt = do
    getByType WHILE
    condition <- between (getByType LPAREN) (getByType RPAREN) expression
    code      <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    return $ WhileStmt condition code

forStmt :: Parser Statement
forStmt = do
    getByType FOR >> getByType LPAREN
    element <- var <* getByType COLON
    list    <- expression <* getByType RPAREN
    code    <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    return $ ForStmt element list code

doStmt :: Parser Statement
doStmt = do
    getByType DO
    code <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    getByType WHILE
    condition <- between (getByType LPAREN) (getByType RPAREN) expression
    return $ DoStmt code condition

switchStmt :: Parser Statement
switchStmt = do
    getByType SWITCH
    idf <- between (getByType LPAREN) (getByType RPAREN) getIdentifier
    getByType LBRACE
    cases <- some caseStmt
    def   <- optional (getByType DEFAULT >> getByType COLON >> some statement)
    return $ SwitchStmt idf cases def


caseStmt :: Parser Statement
caseStmt = do
    getByType CASE
    expr <-
        getByType LPAREN >> expression <* getByType RPAREN <* getByType COLON
    code <- many statement
    return $ CaseStmt expr code

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> (expression <* getByType SEMICOLON)


expression :: Parser Expression
expression = try assignExpr

assignExpr :: Parser Expression
assignExpr = do
    expr <- try orExpr <|> try callExpr
    case expr of
        CallExpr _ _ -> AssignExpr expr <$> (getByType ASSIGN *> assignExpr)
        _            -> return expr

orExpr :: Parser Expression
orExpr = do
    lhs <- andExpr
    op  <- optional (getByType OR)
    case op of
        Just _  -> BinaryExpr lhs OR <$> andExpr
        Nothing -> return lhs

andExpr :: Parser Expression
andExpr = do
    lhs <- equalityExpr
    op  <- optional (getByType AND)
    case op of
        Just _  -> BinaryExpr lhs OR <$> equalityExpr
        Nothing -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- relationExpr
    op  <- optional (try (getByType EQUALS) <|> try (getByType NEQUALS))
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> relationExpr
        Nothing -> return lhs

relationExpr :: Parser Expression
relationExpr = do
    lhs <- additionExpr
    op  <- optional
        (   try (getByType LESS_EQUALS)
        <|> try (getByType GREATER_EQUALS)
        <|> try (getByType LESS)
        <|> try (getByType GREATER)
        )
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> additionExpr
        Nothing -> return lhs

additionExpr :: Parser Expression
additionExpr = do
    lhs <- multiplicationExpr
    op  <- optional (try (getByType PLUS) <|> try (getByType MINUS))
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> multiplicationExpr
        Nothing -> return lhs

multiplicationExpr :: Parser Expression
multiplicationExpr = do
    lhs <- unaryExpr
    op  <- optional (try (getByType MUL) <|> try (getByType DIV))
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> unaryExpr
        Nothing -> return lhs

unaryExpr :: Parser Expression
unaryExpr = do
    op <- optional (try (getByType EXCL_MARK) <|> try (getByType MINUS))
    case op of
        Just x  -> UnaryExpr (typ x) <$> unaryExpr
        Nothing -> try groupingExpr <|> try callExpr


callExpr :: Parser Expression
callExpr = do
    base <- getIdentifier
    idfs <- many $ getByType DOT >> getIdentifier
    temp <- optional $ getByType LPAREN
    case temp of
        Nothing -> return $ CallExpr (base : idfs) []
        Just x  -> CallExpr (base : idfs) <$> getArgs

baseExpr :: Parser Expression
baseExpr = do
    tok <- choice
        [ getByType NUM
        , getByType STRING
        , getByType TRUE
        , getByType FALSE
        , getByType NULL
        , getByType IDF
        ]
    return $ BaseExpr tok


groupingExpr :: Parser Expression
groupingExpr = between (getByType LPAREN) (getByType RPAREN) expression


getArgs :: Parser [Expression]
getArgs = do
    base <- option NullExpr expression
    case base of
        NullExpr -> return []
        _        -> idfs
    where idfs = many $ getByType COMMA >> expression


parameters :: Parser [(Text, Text)]
parameters = do
    par   <- var
    comma <- optional $ getByType COMMA
    case comma of
        Nothing -> return [par]
        Just x  -> parameters

var :: Parser (Text, Text)
var = do

    typ  <- getIdentifier

    name <- getIdentifier

    return (typ, name)


getIdentifier :: Parser Text
getIdentifier = do
    idf <- getByType IDF
    return $ txt idf

--getString :: Parser Text
--getString = do
--    T.pack <$> between (getByType TType) (char '"') (many (text))
--    where text = satisfy (/= '"')
