{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Data.Maybe
import           Data.Data
import qualified Data.Set                      as Set
import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Read

import           Text.Megaparsec         hiding ( single )
import qualified Text.Megaparsec               as MP
                                                ( single )
import           Text.Megaparsec.Debug

import           Parser.Token
import           PLexer



type Parser = Parsec Void PStream

skipSpace :: Parser ()
skipSpace =
    L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: Tokens Text -> Parser Text
symbol = L.symbol skipSpace

newtype Program = Program [Declaration]
    deriving(Show, Eq)

data Declaration =
      System Expression (Maybe Expression) Block
    | Container Expression (Maybe Expression) Block
    | Datatype Expression (Maybe Expression) Block
    | Var Expression Expression
    | Function Expression Expression [(Expression, Expression)] Block
    | Library Expression Block
    | Stmt Statement
    deriving(Show, Eq)

data Statement =
      PPStmt PPDirective
    | BlockStmt Block
    | IfStmt Expression [Statement] [Elif] [Statement]
    | WhileStmt Expression [Statement]
    | ForStmt Expression Expression [Statement]
    | DoStmt [Statement] Expression
    | SwitchStmt Expression [Statement] [Statement]
    | CaseStmt Expression [Statement]
    | ExprStmt Expression
    deriving(Show, Eq)

data PPDirective =
      Include Expression
    | FromIncl Expression Expression
    | Safety Integer
    deriving(Show, Eq)

data Block = Block [Declaration] | EmptyBlock
    deriving(Show, Eq)

data Elif = Elif Expression [Statement]
    deriving(Show, Eq)

data Expression =
      AssignExpr Expression Expression
    | BinaryExpr Expression TType Expression
    | BooleanExpr Bool
    | CallExpr [Call]
    | GroupingExpr Expression
    | IdfExpr Text
    | NullExpr
    | NumExpr Double
    | StringExpr Text
    | UnaryExpr TokenType.TokenType Expression
    | BaseExpr TokenType.TokenType
    deriving(Show, Eq)

data Call =  FnCall Expression [Expression] | BaseCall Expression -- ?  basecall - better name?


    deriving(Show, Eq)

program :: Parser Program
program = Program <$> some declaration <* space <* eof

declaration :: Parser Declaration
declaration = space >> choice
    [ try sysDec
    , try contDec
    , try dtypeDec
    , try varDec
    , try functionDec
    , try libDec
    , Stmt <$> statement
    ]


sysDec :: Parser Declaration
sysDec = do
    getByType SYSTEM <?> "system"
    name  <- dbg "system name" (getIdentifier <?> "system name")
    par   <- dbg "system parent" parent
    block <- dbg "system block" blockStmt
    return $ System name par block


contDec :: Parser Declaration
contDec = do
    getByType CONTAINER <?> "container"
    name <- getIdentifier
    par  <- parent
    Container name par <$> blockStmt

dtypeDec :: Parser Declaration
dtypeDec = do
    getByType DATATYPE
    name <- getIdentifier
    par  <- parent
    Datatype name par <$> blockStmt

parent :: Parser (Maybe Text)
parent =
    dbg "parent"
        $  optional
        $  try
        $  getByType LESS
        >> (getIdentifier <|> failure Nothing (formList))


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
blockStmt = dbg "block" $ Block <$> between (getByType LBRACE)
                                            (getByType RBRACE)
                                            (many declaration)

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


parameters :: Parser [(Expression, Expression)]
parameters = do
    par   <- var
    comma <- optional $ getByType COMMA
    case comma of
        Nothing -> return [par]
        Just x  -> parameters

var :: Parser (Expression, Expression)
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

