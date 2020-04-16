{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Data.Maybe
import           Data.Data
import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import qualified Parser.TokenType              as TokenType


type Parser = Parsec Void Text

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
    | SwitchStmt Text [Statement] [Statement]
    | CaseStmt Expression [Statement]
    | ExprStmt Expression
    deriving(Show, Eq)

data PPDirective =
      Include Text
    | FromIncl Text Text
    | Safety Integer
    deriving(Show, Eq)

data Block = Block [Declaration] | EmptyBlock
    deriving(Show, Eq)

data Elif = Elif Expression [Statement]
    deriving(Show, Eq)

data Expression =
      AssignExpr Expression Expression
    | BinaryExpr Expression TokenType.TokenType Expression
    | BooleanExpr Bool
    | CallExpr [Call]
    | GroupingExpr Expression
    | IdentifierExpr Text
    | NullExpr
    | NumExpr Double
    | StringExpr Text
    | UnaryExpr TokenType.TokenType Expression
    | BaseExpr Text
    deriving(Show, Eq)

data Call = IdfCall Text | FnCall Text [Expression] | BaseCall Expression
    deriving(Show, Eq)

program :: Parser [Declaration]
program = some declaration <* space <* eof

declaration :: Parser Declaration
declaration =
    space
        >> (   try sysDec
           <|> try contDec
           <|> try dtypeDec
           <|> try varDec
           <|> try functionDec
           <|> try libDec
           <|> try (Stmt <$> statement)
           )


sysDec :: Parser Declaration
sysDec = do
    string "system"
    space
    name <- getIdentifier
    space
    par <- parent
    space
    System name par <$> blockStmt


contDec :: Parser Declaration
contDec = do
    string "container"
    space
    name <- getIdentifier
    space
    par <- parent
    space
    Container name par <$> blockStmt

dtypeDec :: Parser Declaration
dtypeDec = do
    string "datatype"
    space
    name <- getIdentifier
    space
    par <- parent
    space
    Datatype name par <$> blockStmt

parent :: Parser Text
parent = option "" (try $ char '<' >> space >> getIdentifier)


varDec :: Parser Declaration
varDec = do
    string "var" >> space
    typ  <- getIdentifier <* space
    name <- getIdentifier <* space <* char ';'
    return $ Var typ name

functionDec :: Parser Declaration
functionDec = do
    string "fn" <* space
    typ    <- getIdentifier <* space
    name   <- getIdentifier <* space
    params <- between (char '(') (char ')') $ option [] $ try parameters
    space
    Function typ name params <$> blockStmt

libDec :: Parser Declaration
libDec = do
    string "library"
    space
    name <- getIdentifier
    space
    Library name <$> blockStmt


statement :: Parser Statement
statement =
    try preProcessorStmt
        <|> try ifStmt
        <|> try whileStmt
        <|> try forStmt
        <|> try doStmt
        <|> try switchStmt
        <|> try exprStmt

preProcessorStmt :: Parser Statement
preProcessorStmt = char '#' >> space >> PPStmt <$> ppDirective

ppDirective :: Parser PPDirective
ppDirective =
    try include
        <|> try fromIncl
        <|> try safety
        <?> "expecting a preprocessor directive"

include :: Parser PPDirective
include = string "include" >> space1 >> Include <$> getIdentifier

fromIncl :: Parser PPDirective
fromIncl = do
    string "from" <* space
    lib <- getIdentifier <* space
    string "include" <* space
    FromIncl lib <$> getIdentifier

safety :: Parser PPDirective
safety = do
    try (string "safety")
    space
    level <- oneOf ['0', '1', '2'] <?> "'0', '1' or '2'"
    space
    return $ Safety $ read [level]


blockStmt :: Parser Block
blockStmt = do
    space
    char '{'
    space
    dec <- manyTill declaration (char '}')
    space
    return $ Block dec

ifStmt :: Parser Statement
ifStmt = do
    string "if" <* space
    condition <- between (char '(' >> space) (space >> char ')') expression
    space >> char '{' >> space
    code <- some statement
    space >> char '}' >> space
    elifBlocks <- many elif
    space
    els <- optional $ string "else"
    space
    case els of
        Nothing -> return $ IfStmt condition code elifBlocks []
        Just x  -> IfStmt condition code elifBlocks <$> some statement


elif :: Parser Elif
elif = do
    try $ string "elif"
    space
    condition <- between (char '(') (char ')') expression
    space
    char '{'
    space
    code <- some statement
    space
    char '}'
    space
    return $ Elif condition code


whileStmt :: Parser Statement
whileStmt = do
    try $ string "while" <* space1
    condition <- between (char '(') (char ')') expression
    space
    char '{'
    space
    code <- some statement
    space
    char '}'
    space
    return $ WhileStmt condition code

forStmt :: Parser Statement
forStmt = do
    try $ string "for"
    space >> char '(' >> space
    element <- var
    space >> char ':' >> space
    list <- expression
    space >> char ')' >> space
    char '{' >> space
    code <- some statement
    space >> char '}' >> space
    return $ ForStmt element list code

doStmt :: Parser Statement
doStmt = do
    try $ string "do"
    space >> char '{' >> space
    code <- some statement
    space >> char '}' >> space
    string "while"
    space >> char '(' >> space
    condition <- expression
    space >> char ')' >> space
    return $ DoStmt code condition

switchStmt :: Parser Statement
switchStmt = do
    try $ string "switch"
    space >> char '(' >> space
    idf <- getIdentifier
    space >> char ')' >> space
    char '{' >> space
    cases   <- some caseStmt
    testDef <- optional (string "default")
    case testDef of
        Just x -> do
            space >> char ':' >> space
            def <- some statement
            space >> char '}' >> space
            return $ SwitchStmt idf cases def
        Nothing -> return $ SwitchStmt idf cases []

caseStmt :: Parser Statement
caseStmt = do
    try $ string "case"
    space >> char '(' >> space
    expr <- expression
    space >> char ')' >> space >> char ':' >> space
    code <- many statement
    return $ CaseStmt expr code

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> (space >> expression <* space <* char ';' <* space)


expression :: Parser Expression
expression = try assignExpr

assignExpr :: Parser Expression
assignExpr = do
    space
    expr <- orExpr <* space
    op   <- option ' ' (char '=')
    space
    case op of
        '=' -> AssignExpr expr <$> orExpr
        _   -> return expr

orExpr :: Parser Expression
orExpr = do
    lhs <- andExpr <* space
    op  <- option "" (try (string "||"))
    space
    case op of
        "||" -> BinaryExpr lhs TokenType.OR <$> orExpr
        _    -> return lhs

andExpr :: Parser Expression
andExpr = do
    lhs <- equalityExpr <* space
    op  <- option "" (string "&&")
    space
    case op of
        "&&" -> BinaryExpr lhs TokenType.AND <$> andExpr
        _    -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- relationExpr <* space
    op  <- option "" (try (string "==") <|> try (string "!="))
    space
    case op of
        "==" -> BinaryExpr lhs TokenType.EQUALS <$> equalityExpr
        "!=" -> BinaryExpr lhs TokenType.NEQUALS <$> equalityExpr
        _    -> return lhs

relationExpr :: Parser Expression
relationExpr = do
    lhs <- additionExpr <* space
    op  <- option
        ""
        (try (string "<=") <|> try (string ">=") <|> try (string "<") <|> try
            (string ">")
        )
    space
    case op of
        "<=" -> BinaryExpr lhs TokenType.LESS_EQUALS <$> relationExpr
        ">=" -> BinaryExpr lhs TokenType.GREATER_EQUALS <$> relationExpr
        "<"  -> BinaryExpr lhs TokenType.LESS <$> relationExpr
        ">"  -> BinaryExpr lhs TokenType.GREATER <$> relationExpr
        _    -> return lhs

additionExpr :: Parser Expression
additionExpr = do
    lhs <- multiplicationExpr <* space
    op  <- option ' ' (try (oneOf ['+', '-']))
    space
    case op of
        '+' -> BinaryExpr lhs TokenType.PLUS <$> additionExpr
        '-' -> BinaryExpr lhs TokenType.MINUS <$> additionExpr
        _   -> return lhs

multiplicationExpr :: Parser Expression
multiplicationExpr = do
    lhs <- unaryExpr <* space
    op  <- option ' ' (try (oneOf ['*', '/']))
    space
    case op of
        '*' -> BinaryExpr lhs TokenType.MUL <$> multiplicationExpr
        '/' -> BinaryExpr lhs TokenType.DIV <$> multiplicationExpr
        _   -> return lhs

unaryExpr :: Parser Expression
unaryExpr = do
    space
    op <- option ' ' (try (oneOf ['!', '-']))
    space
    case op of
        '!' -> UnaryExpr TokenType.EXCL_MARK <$> unaryExpr
        '-' -> UnaryExpr TokenType.MINUS <$> unaryExpr
        _   -> try groupingExpr <|> callExpr

callExpr :: Parser Expression
callExpr = CallExpr <$> getCall

getCall :: Parser [Call]
getCall = do
    base <- optional (lookAhead (try getIdentifier))
    space
    case base of
        Nothing -> do
            b <- baseExpr
            return [BaseCall b]
        Just x -> do
            idf  <- getIdentifier
            next <- lookAhead (try anySingle) <* space
            case next of
                '.' -> do
                    rest <- getCall <* space
                    return (IdfCall idf : rest)
                '(' -> do
                    args <- getArgs <* space <* char ')' <* space
                    rest <- getCall <* space
                    return (FnCall idf args : rest)
                _ -> return [IdfCall idf]

baseExpr :: Parser Expression
baseExpr =
    (BaseExpr <$> try (T.pack <$> some digitChar))
        <|> (BaseExpr <$> try (string "true"))
        <|> (BaseExpr <$> try (string "false"))
        <|> (BaseExpr <$> try (string "null"))
        <|> (BaseExpr <$> try getIdentifier)
        <|> (BaseExpr <$> getString)

groupingExpr :: Parser Expression
groupingExpr = between (char '(') (char ')') expression


getArgs :: Parser [Expression]
getArgs = do
    base <- option NullExpr expression
    case base of
        NullExpr -> return []
        _        -> idfs
    where idfs = many $ char ',' >> expression


parameters :: Parser [(Text, Text)]
parameters = do
    par   <- var
    comma <- optional $ char ','
    case comma of
        Nothing -> return [par]
        Just x  -> parameters <?> "expecting parameter"

var :: Parser (Text, Text)
var = do
    space
    typ  <- getIdentifier <* space
    name <- getIdentifier <* space
    return (typ, name)


getIdentifier :: Parser Text
getIdentifier = T.pack <$> some letterChar <> many alphaNumChar

getString :: Parser Text
getString = do
    T.pack <$> between (char '"') (char '"') (many text)
    where text = satisfy (/= '"')
