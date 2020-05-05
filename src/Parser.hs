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
    | BinaryExpr Expression TokenType.TokenType Expression
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

parent :: Parser (Maybe Expression)
parent = optional (try $ symbol "<" >> getIdentifier)


varDec :: Parser Declaration
varDec = do
    symbol "var"
    (typ, name) <- var <* symbol ";"
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
    symbol ";"
    return $ DoStmt code condition

switchStmt :: Parser Statement
switchStmt = do
    symbol "switch"
    idf <- between (symbol "(") (symbol ")") getIdentifier
    symbol "{"
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
    expr <- expression <* symbol ":"
    code <- manyTill
        statement
        (choice [lookAhead (symbol "case"), lookAhead (symbol "default")])
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
    case op of
        "||" -> BinaryExpr lhs TokenType.OR <$> orExpr
        _    -> return lhs

andExpr :: Parser Expression
andExpr = do
    lhs <- lexeme equalityExpr
    op  <- option "" (symbol "&&")
    case op of
        "&&" -> BinaryExpr lhs TokenType.AND <$> andExpr
        _    -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- lexeme relationExpr
    op  <- option "" $ choice [symbol "==", symbol "!="]
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
    base <- baseExpr
    case base of
        IdfExpr _ -> do
            next <- option ' ' $ try $ lexeme (oneOf ['(', '.'])
            case next of
                '.' -> do
                    rest <- lexeme getCall
                    return (BaseCall base : rest)
                '(' -> do
                    args <- getArgs <* space <* char ')' <* space
                    rest <- getCall <* space
                    return (FnCall base args : rest)
                _ -> return [BaseCall base]
        _ -> return [BaseCall base]

baseExpr :: Parser Expression
baseExpr =
    (NumExpr <$> choice [try (lexeme L.float), try (lexeme L.decimal)])
        <|> (BaseExpr TokenType.TRUE <$ try (symbol "true"))
        <|> (BaseExpr TokenType.FALSE <$ try (symbol "false"))
        <|> (BaseExpr TokenType.NULL <$ try (symbol "null"))
        <|> try getIdentifier
        <|> (StringExpr <$> getString)

groupingExpr :: Parser Expression
groupingExpr = between (char '(') (char ')') expression


getArgs :: Parser [Expression]
getArgs = do
    base <- option NullExpr expression
    case base of
        NullExpr -> return []
        _        -> idfs
    where idfs = many $ char ',' >> expression


parameters :: Parser [(Expression, Expression)]
parameters = do
    par   <- var
    comma <- optional $ symbol ","
    case comma of
        Nothing -> return [par]
        Just x  -> do
            rest <- parameters <?> "parameters"
            return (par : rest)

var :: Parser (Expression, Expression)
var = do
    space
    typ  <- getIdentifier
    name <- getIdentifier
    return (typ, name)


getIdentifier :: Parser Expression
getIdentifier = IdfExpr . T.pack <$> lexeme
    (some validFirst <> many validOther)
  where
    validFirst = choice [letterChar, char '_']
    validOther = choice [alphaNumChar, char '_']

getString :: Parser Text
getString = T.pack <$> between (symbol "\"") (symbol "\"") (many text)
    where text = satisfy (/= '"')
