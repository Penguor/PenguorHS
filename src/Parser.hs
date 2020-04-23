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
    | ForStmt Text Expression [Statement]
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
    symbol "system"
    name <- getIdentifier
    par  <- parent
    System name par <$> blockStmt


contDec :: Parser Declaration
contDec = do
    symbol "container"
    name <- getIdentifier
    par  <- parent
    Container name par <$> blockStmt

dtypeDec :: Parser Declaration
dtypeDec = do
    symbol "datatype"
    name <- getIdentifier
    par  <- parent
    Datatype name par <$> blockStmt

parent :: Parser Text
parent = option "" (try $ symbol "<" >> getIdentifier)


varDec :: Parser Declaration
varDec = do
    symbol "var"
    typ  <- getIdentifier
    name <- getIdentifier <* symbol ";"
    return $ Var typ name

functionDec :: Parser Declaration
functionDec = do
    symbol "fn"
    typ    <- getIdentifier
    name   <- getIdentifier
    params <- between (symbol "(") (symbol ")") $ option [] $ try parameters
    Function typ name params <$> blockStmt

libDec :: Parser Declaration
libDec = do
    symbol "library"
    name <- getIdentifier
    Library name <$> blockStmt


statement :: Parser Statement
statement = choice
    [ try preProcessorStmt
    , try ifStmt
    , try whileStmt
    , try forStmt
    , try doStmt
    , try switchStmt
    , exprStmt
    ]

preProcessorStmt :: Parser Statement
preProcessorStmt = symbol "#" >> PPStmt <$> ppDirective

ppDirective :: Parser PPDirective
ppDirective =
    try include <|> try fromIncl <|> safety <?> "preprocessor directive"

include :: Parser PPDirective
include = symbol "include" >> Include <$> getIdentifier

fromIncl :: Parser PPDirective
fromIncl = do
    symbol "from"
    lib <- getIdentifier
    symbol "include"
    FromIncl lib <$> getIdentifier

safety :: Parser PPDirective
safety = do
    symbol "safety"
    level <- lexeme (oneOf ['0', '1', '2'] <?> "'0', '1' or '2'")
    return $ Safety $ read [level]


blockStmt :: Parser Block
blockStmt = do
    symbol "{"
    dec <- manyTill declaration (symbol "}")
    return $ Block dec

ifStmt :: Parser Statement
ifStmt = do
    symbol "if"
    condition  <- between (symbol "(") (symbol ")") expression
    code       <- between (symbol "{") (symbol "}") (some statement)
    elifBlocks <- many elif
    els        <- optional $ symbol "else"
    case els of
        Nothing -> return $ IfStmt condition code elifBlocks []
        Just x  -> IfStmt condition code elifBlocks <$> some statement


elif :: Parser Elif
elif = do
    symbol "elif"
    condition <- between (symbol "(") (symbol ")") expression
    code      <- between (symbol "{") (symbol "}") (some statement)
    return $ Elif condition code


whileStmt :: Parser Statement
whileStmt = do
    symbol "while"
    condition <- between (symbol "(") (symbol ")") expression
    code      <- between (symbol "{") (symbol "}") (some statement)
    return $ WhileStmt condition code

forStmt :: Parser Statement
forStmt = do
    try $ string "for"
    symbol "("
    element <- getIdentifier <* symbol ":"
    list    <- expression
    symbol ")"
    code <- between (symbol "{") (symbol "}") (some statement)
    return $ ForStmt element list code

doStmt :: Parser Statement
doStmt = do
    symbol "do"
    code <- between (symbol "{") (symbol "}") (some statement)
    symbol "while"
    condition <- between (symbol "(") (symbol ")") expression
    return $ DoStmt code condition

switchStmt :: Parser Statement
switchStmt = do
    symbol "switch"
    idf <- between (symbol "(") (symbol ")") getIdentifier
    symbol "}"
    cases   <- some caseStmt
    testDef <- optional (string "default")
    case testDef of
        Just x -> do
            symbol ":"
            def <- some statement
            symbol "}"
            return $ SwitchStmt idf cases def
        Nothing -> return $ SwitchStmt idf cases []

caseStmt :: Parser Statement
caseStmt = do
    symbol "case"
    expr <- between (symbol "(") (symbol ")") expression
    code <- many statement
    return $ CaseStmt expr code

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> (lexeme expression <* symbol ";")


expression :: Parser Expression
expression = assignExpr

assignExpr :: Parser Expression
assignExpr = do
    expr <- lexeme orExpr
    op   <- option "" (symbol "=")
    case op of
        "=" -> AssignExpr expr <$> orExpr
        _   -> return expr

orExpr :: Parser Expression
orExpr = do
    lhs <- lexeme andExpr
    op  <- option "" (symbol "||")
    space
    case op of
        "||" -> BinaryExpr lhs TokenType.OR <$> orExpr
        _    -> return lhs

andExpr :: Parser Expression
andExpr = do
    lhs <- lexeme equalityExpr
    op  <- option "" (symbol "&&")
    space
    case op of
        "&&" -> BinaryExpr lhs TokenType.AND <$> andExpr
        _    -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- lexeme relationExpr
    op  <- option "" $ choice [symbol "==", symbol "!="]
    space
    case op of
        "==" -> BinaryExpr lhs TokenType.EQUALS <$> equalityExpr
        "!=" -> BinaryExpr lhs TokenType.NEQUALS <$> equalityExpr
        _    -> return lhs

relationExpr :: Parser Expression
relationExpr = do
    lhs <- additionExpr <* space
    op  <- option "" $ choice [symbol "<=", symbol ">=", symbol "<", symbol ">"]
    space
    case op of
        "<=" -> BinaryExpr lhs TokenType.LESS_EQUALS <$> relationExpr
        ">=" -> BinaryExpr lhs TokenType.GREATER_EQUALS <$> relationExpr
        "<"  -> BinaryExpr lhs TokenType.LESS <$> relationExpr
        ">"  -> BinaryExpr lhs TokenType.GREATER <$> relationExpr
        _    -> return lhs

additionExpr :: Parser Expression
additionExpr = do
    lhs <- lexeme multiplicationExpr
    op  <- option ' ' (try (oneOf ['+', '-']))
    space
    case op of
        '+' -> BinaryExpr lhs TokenType.PLUS <$> additionExpr
        '-' -> BinaryExpr lhs TokenType.MINUS <$> additionExpr
        _   -> return lhs

multiplicationExpr :: Parser Expression
multiplicationExpr = do
    lhs <- lexeme unaryExpr
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
    comma <- optional $ symbol ","
    case comma of
        Nothing -> return [par]
        Just x  -> do
            rest <- parameters <?> "parameters"
            return (par : rest)

var :: Parser (Text, Text)
var = do
    space
    typ  <- getIdentifier
    name <- getIdentifier
    return (typ, name)


getIdentifier :: Parser Text
getIdentifier = T.pack <$> lexeme (some letterChar <> many alphaNumChar)

getString :: Parser Text
getString = T.pack <$> between (char '"') (char '"') (many text)
    where text = satisfy (/= '"')
