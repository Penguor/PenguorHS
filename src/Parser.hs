module Parser where


import           Data.Maybe
import           Data.Data
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lexer

import qualified Parser.TokenType              as TokenType


type Parser = Parsec Void String

data Declaration =
      System String String Block
    | Container String String Block
    | Datatype String String Block
    | Var String String
    | Function String String [(String, String)] Block
    | Library String Block
    | Stmt Statement
    deriving(Show, Eq)

data Statement =
      PPStmt PPDirective
    | BlockStmt Block
    | IfStmt Expression [Statement] [Elif] [Statement]
    | WhileStmt Expression [Statement]
    | ForStmt (String, String) Expression [Statement]
    | DoStmt [Statement] Expression
    | SwitchStmt String [Statement] [Statement]
    | CaseStmt Expression [Statement]
    | ExprStmt Expression
    deriving(Show, Eq)

data PPDirective =
      Include String
    | FromIncl String String
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
    | CallExpr [String] [Expression]
    | GroupingExpr Expression
    | IdentifierExpr String
    | NullExpr
    | NumExpr Double
    | StringExpr String
    | UnaryExpr TokenType.TokenType Expression
    | BaseExpr String
    deriving(Show, Eq)

program :: Parser [Declaration]
program = some declaration

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

parent :: Parser (String)
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
    level <- oneOf "012"
    space
    return $ Safety $ read [level]


blockStmt :: Parser Block
blockStmt = do
    space
    char '{' <?> "Expected '{'"
    space
    dec <- manyTill declaration (char '}') <?> "error in BlockStmt"
    space
    return $ Block dec

ifStmt :: Parser Statement
ifStmt = do
    string "if"
    space
    condition <- between (char '(') (char ')') expression
    space
    char '{'
    space
    code <- some statement
    space
    char '}'
    space
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
exprStmt = ExprStmt <$> (expression <* space <* char ';')


expression :: Parser Expression
expression = try assignExpr <?> "expression"

assignExpr :: Parser Expression
assignExpr = do
    expr <- try orExpr <|> try callExpr <?> "expecting || or call"
    space
    case expr of
        CallExpr _ _ -> AssignExpr expr <$> (char '=' *> space *> assignExpr)
        _            -> return expr

orExpr :: Parser Expression
orExpr = do
    lhs <- andExpr
    op  <- option "" (string "||")
    case op of
        "||" -> BinaryExpr lhs TokenType.OR <$> andExpr
        _    -> return lhs

andExpr :: Parser Expression
andExpr = do
    lhs <- equalityExpr
    op  <- option "" (string "&&")
    case op of
        "&&" -> BinaryExpr lhs TokenType.AND <$> equalityExpr
        _    -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- relationExpr
    op  <- option "" (try (string "==") <|> try (string "!="))
    case op of
        "==" -> BinaryExpr lhs TokenType.EQUALS <$> relationExpr
        "!=" -> BinaryExpr lhs TokenType.NEQUALS <$> relationExpr
        _    -> return lhs

relationExpr :: Parser Expression
relationExpr = do
    lhs <- additionExpr
    op  <- option
        ""
        (try (string "<=") <|> try (string ">=") <|> try (string "<") <|> try
            (string ">")
        )
    case op of
        "<=" -> BinaryExpr lhs TokenType.LESS_EQUALS <$> additionExpr
        ">=" -> BinaryExpr lhs TokenType.GREATER_EQUALS <$> additionExpr
        "<"  -> BinaryExpr lhs TokenType.LESS <$> additionExpr
        ">"  -> BinaryExpr lhs TokenType.GREATER <$> additionExpr
        _    -> return lhs

additionExpr :: Parser Expression
additionExpr = do
    lhs <- multiplicationExpr
    op  <- option ' ' (try (oneOf "+-"))
    case op of
        '+' -> BinaryExpr lhs TokenType.PLUS <$> multiplicationExpr
        '-' -> BinaryExpr lhs TokenType.MINUS <$> multiplicationExpr
        _   -> return lhs

multiplicationExpr :: Parser Expression
multiplicationExpr = do
    lhs <- unaryExpr
    op  <- option ' ' (try (oneOf "*/"))
    case op of
        '*' -> BinaryExpr lhs TokenType.MUL <$> unaryExpr
        '/' -> BinaryExpr lhs TokenType.DIV <$> unaryExpr
        _   -> return lhs

unaryExpr :: Parser Expression
unaryExpr = do
    op <- option ' ' (try (oneOf "!-"))
    case op of
        '!' -> UnaryExpr TokenType.EXCL_MARK <$> unaryExpr
        '-' -> UnaryExpr TokenType.MINUS <$> unaryExpr
        _   -> try groupingExpr <|> try callExpr


callExpr :: Parser Expression
callExpr = do
    base <- getIdentifier
    idfs <- many $ char '.' >> getIdentifier

    temp <- optional $ char '('
    case temp of
        Nothing -> return $ CallExpr (base : idfs) []
        Just x  -> CallExpr (base : idfs) <$> getArgs

baseExpr :: Parser Expression
baseExpr =
    BaseExpr
        <$> try (some digitChar)
        <|> BaseExpr
        <$> try (string "true")
        <|> BaseExpr
        <$> try (string "null")
        <|> BaseExpr
        <$> try getIdentifier
        <?> "Expected base expression"

groupingExpr :: Parser Expression
groupingExpr = between (char '(') (char ')') expression


getArgs :: Parser [Expression]
getArgs = do
    base <- option NullExpr expression
    case base of
        NullExpr -> return []
        _        -> idfs
    where idfs = many $ char ',' >> expression


parameters :: Parser [(String, String)]
parameters = do
    par   <- var
    comma <- optional $ char ','
    case comma of
        Nothing -> return [par]
        Just x  -> parameters <?> "expecting parameter"

var :: Parser (String, String)
var = do
    space
    typ <- getIdentifier
    space
    name <- getIdentifier
    space
    return (typ, name)


getIdentifier :: Parser String
getIdentifier = some letterChar <> many alphaNumChar

getString :: Parser String
getString = do
    satisfy (== '"')
    str <- many text
    satisfy (== '"')
    return str
    where text = satisfy (/= '"')


