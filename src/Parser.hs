{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Data.Maybe
import           Data.Data
import qualified Data.Set                      as Set
import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Read
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec
import           Text.Megaparsec.Debug

import           Parser.Token
import           PLexer


type Parser = Parsec Void PStream

newtype Program = Program [Declaration]
    deriving(Show, Eq)

data Declaration =
      System Modifier Expression (Maybe Expression) Block
    | Container Modifier Expression (Maybe Expression) Block
    | Datatype Modifier Expression (Maybe Expression) Block
    | Var Modifier Expression Expression (Maybe Expression)
    | Function Modifier Expression Expression [(Expression, Expression)] Block
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
    | SwitchStmt Expression [Statement] (Maybe [Statement])
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
      AssignExpr Expression TType Expression
    | BinaryExpr Expression TType Expression
    | BooleanExpr Bool
    | CallExpr [Call]
    | GroupingExpr Expression
    | IdfExpr Text
    | NullExpr
    | NumExpr Double
    | StringExpr Text
    | UnaryExpr TType Expression
    | BaseExpr TType
    deriving(Show, Eq)

data Call =  FnCall Expression [Expression] | BaseCall Expression -- ?  basecall - better name?
    deriving(Show, Eq)

data Modifier = Mod (Maybe Tok) [Tok]
    deriving(Show, Eq)

program :: Parser Program
program = Program <$> some declaration <* (getByType EOF <?> "end of file")

declaration :: Parser Declaration
declaration = do
    mods <- try getModifiers
    if mods == Mod Nothing []
        then choice
            [ sysDec mods
            , contDec mods
            , dtypeDec mods
            , try $ functionDec mods
            , try $ varDec mods
            , libDec
            , Stmt <$> statement
            ]
        else choice
            [ sysDec mods
            , contDec mods
            , try $ dtypeDec mods
            , try $ functionDec mods
            , varDec      mods
            ]


sysDec :: Modifier -> Parser Declaration
sysDec m = do
    try (getByType SYSTEM <?> "system")
    name <- getIdentifier <?> "system name"
    par  <- parent
    System m name par <$> blockStmt

contDec :: Modifier -> Parser Declaration
contDec m = do
    try (getByType CONTAINER <?> "container")
    name <- getIdentifier
    par  <- parent
    Container m name par <$> blockStmt

dtypeDec :: Modifier -> Parser Declaration
dtypeDec m = do
    try (getByType DATATYPE <?> "data type")
    name <- getIdentifier
    par  <- parent
    Datatype m name par <$> blockStmt

parent :: Parser (Maybe Expression)
parent = optional $ try (getByType LESS >> getIdentifier)


varDec :: Modifier -> Parser Declaration
varDec m = do
    lookAhead
        (try var >> choice [try (getByType ASSIGN), try (getByType SEMICOLON)])
    (typ, name) <- var
    value       <- optional (try $ getByType ASSIGN >> lOrExpr)
    getByType SEMICOLON
    return $ Var m typ name value

functionDec :: Modifier -> Parser Declaration
functionDec m = do
    lookAhead (try var >> try (getByType LPAREN))
    (typ, name) <- var
    params <- between (getByType LPAREN) (getByType RPAREN) $ option [] $ try
        parameters
    Function m typ name params <$> blockStmt

libDec :: Parser Declaration
libDec = do
    try $ getByType LIBRARY
    name <- getIdentifier
    Library name <$> blockStmt

getModifiers :: Parser Modifier
getModifiers = do
    access <- optional
        (   try (getByType PUBLIC)
        <|> try (getByType PRIVATE)
        <|> try (getByType PROTECTED)
        <|> try (getByType RESTRICTED)
        )
    naccess <- many
        (   try (getByType STATIC)
        <|> try (getByType DYNAMIC)
        <|> try (getByType ABSTRACT)
        <|> try (getByType CONST)
        )
    return $ Mod access naccess

statement :: Parser Statement
statement = choice
    [preProcessorStmt, ifStmt, whileStmt, forStmt, doStmt, switchStmt, exprStmt]


preProcessorStmt :: Parser Statement
preProcessorStmt = try $ getByType HASHTAG >> PPStmt <$> ppDirective

ppDirective :: Parser PPDirective
ppDirective =
    choice [try include, try fromIncl, safety] <?> "preprocessor directive"

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
    level <- getByType NUM
    let intL = read (T.unpack (txt level))
    if intL > 2 || intL < 0
        then fail "expecting a number between 0 and 2"
        else return $ Safety intL


blockStmt :: Parser Block
blockStmt =
    Block <$> between (getByType LBRACE) (getByType RBRACE) (many declaration)

ifStmt :: Parser Statement
ifStmt = do
    try $ getByType IF
    condition  <- between (getByType LPAREN) (getByType RPAREN) expression
    code       <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    elifBlocks <- many elif
    els        <- optional (try (getByType ELSE))
    case els of
        Nothing -> return $ IfStmt condition code elifBlocks []
        Just x  -> IfStmt condition code elifBlocks <$> some statement


elif :: Parser Elif
elif = do
    try $ getByType ELIF
    condition <- between (getByType LPAREN) (getByType RPAREN) expression
    code      <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    return $ Elif condition code


whileStmt :: Parser Statement
whileStmt = do
    try $ getByType WHILE
    condition <- between (getByType LPAREN) (getByType RPAREN) expression
    code      <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    return $ WhileStmt condition code

forStmt :: Parser Statement
forStmt = do
    try $ getByType FOR >> getByType LPAREN
    element <- getIdentifier <* getByType COLON
    list    <- expression <* getByType RPAREN
    code    <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    return $ ForStmt element list code

doStmt :: Parser Statement
doStmt = do
    try $ getByType DO
    code <- between (getByType LBRACE) (getByType RBRACE) (some statement)
    getByType WHILE
    condition <- between (getByType LPAREN) (getByType RPAREN) expression
    getByType SEMICOLON
    return $ DoStmt code condition

switchStmt :: Parser Statement
switchStmt = do
    try $ getByType SWITCH
    idf <- between (getByType LPAREN) (getByType RPAREN) getIdentifier
    getByType LBRACE
    cases <- some caseStmt
    def   <- optional $ try (getByType DEFAULT)
    case def of
        Just _ -> do
            getByType COLON
            stmts <- some statement
            getByType RBRACE
            return $ SwitchStmt idf cases (Just stmts)
        Nothing -> do
            getByType RBRACE
            return $ SwitchStmt idf cases Nothing


caseStmt :: Parser Statement
caseStmt = do
    try $ getByType CASE
    expr <- expression <* getByType COLON
    code <- many $ try statement
    return $ CaseStmt expr code

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> (expression <* getByType SEMICOLON)

expression :: Parser Expression
expression = assignExpr

assignExpr :: Parser Expression
assignExpr = do
    expr <- lOrExpr
    op   <- optional
        (   try (getByType ASSIGN)
        <|> try (getByType ADD_ASSIGN)
        <|> try (getByType SUB_ASSIGN)
        <|> try (getByType MUL_ASSIGN)
        <|> try (getByType DIV_ASSIGN)
        <|> try (getByType BW_OR_ASSIGN)
        <|> try (getByType BW_XOR_ASSIGN)
        <|> try (getByType BW_AND_ASSIGN)
        <|> try (getByType BS_LEFT_ASSIGN)
        <|> try (getByType BS_RIGHT_ASSIGN)
        )
    case op of
        Just x  -> AssignExpr expr (typ x) <$> lOrExpr
        Nothing -> return expr

lOrExpr :: Parser Expression
lOrExpr = do
    lhs <- lXorExpr
    op  <- optional $ try (getByType OR)
    case op of
        Just _  -> BinaryExpr lhs OR <$> lOrExpr
        Nothing -> return lhs

lXorExpr :: Parser Expression
lXorExpr = do
    lhs <- lAndExpr
    op  <- optional $ try (getByType XOR)
    case op of
        Just _  -> BinaryExpr lhs XOR <$> lXorExpr
        Nothing -> return lhs

lAndExpr :: Parser Expression
lAndExpr = do
    lhs <- bwOrExpr
    op  <- optional $ try (getByType AND)
    case op of
        Just _  -> BinaryExpr lhs AND <$> lAndExpr
        Nothing -> return lhs

bwOrExpr :: Parser Expression
bwOrExpr = do
    lhs <- bwXorExpr
    op  <- optional $ try (getByType BW_OR)
    case op of
        Just _  -> BinaryExpr lhs BW_OR <$> bwOrExpr
        Nothing -> return lhs

bwXorExpr :: Parser Expression
bwXorExpr = do
    lhs <- bwAndExpr
    op  <- optional $ try (getByType BW_XOR)
    case op of
        Just _  -> BinaryExpr lhs BW_XOR <$> bwXorExpr
        Nothing -> return lhs

bwAndExpr :: Parser Expression
bwAndExpr = do
    lhs <- equalityExpr
    op  <- optional $ try (getByType BW_AND)
    case op of
        Just _  -> BinaryExpr lhs BW_AND <$> bwAndExpr
        Nothing -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- relationExpr
    op  <- optional (try (getByType EQUALS) <|> try (getByType NEQUALS))
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> equalityExpr
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
        Just x  -> BinaryExpr lhs (typ x) <$> relationExpr
        Nothing -> return lhs

additionExpr :: Parser Expression
additionExpr = do
    lhs <- multiplicationExpr
    op  <- optional (try (getByType PLUS) <|> try (getByType MINUS))
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> additionExpr
        Nothing -> return lhs

multiplicationExpr :: Parser Expression
multiplicationExpr = do
    lhs <- unaryExpr
    op  <- optional (try (getByType MUL) <|> try (getByType DIV))
    case op of
        Just x  -> BinaryExpr lhs (typ x) <$> multiplicationExpr
        Nothing -> return lhs

unaryExpr :: Parser Expression
unaryExpr = do
    op <- optional
        (try (getByType EXCL_MARK) <|> try (getByType MINUS) <|> try
            (getByType BW_NOT)
        )
    case op of
        Just x  -> UnaryExpr (typ x) <$> unaryExpr
        Nothing -> try groupingExpr <|> try callExpr


callExpr :: Parser Expression
callExpr = CallExpr <$> getCall

getCall :: Parser [Call]
getCall = do
    base <- baseExpr
    case base of
        IdfExpr _ -> do
            next <- optional $ try (getType LPAREN <|> getType DOT)
            case next of
                Just DOT -> do
                    rest <- getCall
                    return (BaseCall base : rest)
                Just LPAREN -> do
                    args <- getArgs <* getByType RPAREN
                    rest <- getCall
                    return (FnCall base args : rest)
                Nothing -> return [BaseCall base]
        _ -> return [BaseCall base]

baseExpr :: Parser Expression
baseExpr = choice
    [ try $ NumExpr . read . T.unpack . txt <$> getByType NUM
    , try $ StringExpr . txt <$> getByType STRING
    , try $ BaseExpr . typ <$> getByType TRUE
    , try $ BaseExpr . typ <$> getByType FALSE
    , try $ BaseExpr . typ <$> getByType NULL
    , IdfExpr . txt <$> getByType IDF
    ]


groupingExpr :: Parser Expression
groupingExpr = between (getByType LPAREN) (getByType RPAREN) expression


getArgs :: Parser [Expression]
getArgs = do
    base <- option NullExpr expression -- ! seems to be incorrect











    case base of
        NullExpr -> return []
        _        -> idfs
    where idfs = many $ getByType COMMA >> expression


parameters :: Parser [(Expression, Expression)]
parameters = do
    par   <- var
    comma <- optional $ try (getByType COMMA)
    case comma of
        Nothing -> return [par]
        Just x  -> do
            rest <- parameters <?> "parameters"
            return (par : rest)

var :: Parser (Expression, Expression)
var = do
    typ  <- getIdentifier
    name <- getIdentifier
    return (typ, name)


getIdentifier :: Parser Expression
getIdentifier = do
    idf <- getByType IDF
    return $ IdfExpr (txt idf)

--getString :: Parser Text
--getString = do
--    T.pack <$> between (getByType TType) (char '"') (many (text))
--    where text = satisfy (/= '"')












