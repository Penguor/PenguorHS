module Parser where


import           Data.Maybe
import qualified Parser.TokenType              as TokenType
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Char
import           Data.Data

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
    | CallExpr [Call]
    | GroupingExpr Expression
    | IdentifierExpr String
    | NullExpr
    | NumExpr Double
    | StringExpr String
    | UnaryExpr TokenType.TokenType Expression
    | BaseExpr String
    deriving(Show, Eq)

data Call = IdfCall String | FnCall String [Expression] | BaseCall Expression
    deriving(Show, Eq)

program :: Parser [Declaration]
program = many1 declaration

declaration :: Parser Declaration
declaration =
    try sysDec
        <|> try contDec
        <|> try dtypeDec
        <|> try varDec
        <|> try functionDec
        <|> try libDec
        <|> try (Stmt <$> statement)


sysDec :: Parser Declaration
sysDec = do
    string "system" <?> "expecting 'system'"
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    System name par <$> blockStmt


contDec :: Parser Declaration
contDec = do
    string "container" <?> "expecting 'container'"
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    Container name par <$> blockStmt

dtypeDec :: Parser Declaration
dtypeDec = do
    string "datatype"
    spaces
    name <- getIdentifier
    spaces
    par <- parent
    spaces
    Datatype name par <$> blockStmt

parent :: Parser (String)
parent = option "" (try $ char '<' >> spaces >> getIdentifier)


varDec :: Parser Declaration
varDec = do
    string "var" >> spaces
    typ  <- getIdentifier <* spaces
    name <- getIdentifier <* spaces <* char ';'
    return $ Var typ name

functionDec :: Parser Declaration
functionDec = do
    string "fn" <* spaces
    typ    <- getIdentifier <* spaces
    name   <- getIdentifier <* spaces
    params <- between (char '(') (char ')') $ option [] $ try parameters
    spaces
    Function typ name params <$> blockStmt

libDec :: Parser Declaration
libDec = do
    string "library"
    spaces
    name <- getIdentifier
    spaces
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
preProcessorStmt = char '#' >> spaces >> PPStmt <$> ppDirective

ppDirective :: Parser PPDirective
ppDirective =
    try include
        <|> try fromIncl
        <|> try safety
        <?> "expecting a preprocessor directive"

include :: Parser PPDirective
include = string "include" >> oneSpaces >> Include <$> getIdentifier

fromIncl :: Parser PPDirective
fromIncl = do
    try (string "from")
    spaces
    lib <- getIdentifier
    spaces
    string "include"
    spaces
    FromIncl lib <$> getIdentifier

safety :: Parser PPDirective
safety = do
    try (string "safety")
    spaces
    level <- oneOf "012"
    spaces
    return $ Safety $ read [level]


blockStmt :: Parser Block
blockStmt = do
    spaces
    char '{' <?> "Expected '{'"
    spaces
    dec <- manyTill declaration (char '}') <?> "error in BlockStmt"
    spaces
    return $ Block dec

ifStmt :: Parser Statement
ifStmt = do
    string "if" <* spaces
    condition <- between (char '(') (char ')') expression
    spaces >> char '{' >> spaces
    code <- many1 statement
    spaces >> char '}' >> spaces
    elifBlocks <- many elif
    spaces
    els <- optionMaybe $ string "else"
    spaces
    case els of
        Nothing -> return $ IfStmt condition code elifBlocks []
        Just x  -> IfStmt condition code elifBlocks <$> many1 statement


elif :: Parser Elif
elif = do
    try $ string "elif"
    spaces
    condition <- between (char '(') (char ')') expression
    spaces
    char '{'
    spaces
    code <- many1 statement
    spaces
    char '}'
    spaces
    return $ Elif condition code


whileStmt :: Parser Statement
whileStmt = do
    try $ string "while" <* oneSpaces
    condition <- between (char '(') (char ')') expression
    spaces
    char '{'
    spaces
    code <- many1 statement
    spaces
    char '}'
    spaces
    return $ WhileStmt condition code

forStmt :: Parser Statement
forStmt = do
    try $ string "for"
    spaces >> char '(' >> spaces
    element <- var
    spaces >> char ':' >> spaces
    list <- expression
    spaces >> char ')' >> spaces
    char '{' >> spaces
    code <- many1 statement
    spaces >> char '}' >> spaces
    return $ ForStmt element list code

doStmt :: Parser Statement
doStmt = do
    try $ string "do"
    spaces >> char '{' >> spaces
    code <- many1 statement
    spaces >> char '}' >> spaces
    string "while"
    spaces >> char '(' >> spaces
    condition <- expression
    spaces >> char ')' >> spaces
    return $ DoStmt code condition

switchStmt :: Parser Statement
switchStmt = do
    try $ string "switch"
    spaces >> char '(' >> spaces
    idf <- getIdentifier
    spaces >> char ')' >> spaces
    char '{' >> spaces
    cases   <- many1 caseStmt
    testDef <- optionMaybe (string "default")
    case testDef of
        Just x -> do
            spaces >> char ':' >> spaces
            def <- many1 statement
            spaces >> char '}' >> spaces
            return $ SwitchStmt idf cases def
        Nothing -> return $ SwitchStmt idf cases []

caseStmt :: Parser Statement
caseStmt = do
    try $ string "case"
    spaces >> char '(' >> spaces
    expr <- expression
    spaces >> char ')' >> spaces >> char ':' >> spaces
    code <- many statement
    return $ CaseStmt expr code

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> (spaces >> expression <* spaces <* char ';' <* spaces)


expression :: Parser Expression
expression = try assignExpr

assignExpr :: Parser Expression
assignExpr = do
    spaces
    expr <- orExpr <* spaces
    op   <- option ' ' (char '=')
    spaces
    case op of
        '=' -> AssignExpr expr <$> orExpr
        _   -> return expr

orExpr :: Parser Expression
orExpr = do
    lhs <- andExpr <* spaces
    op  <- option "" (try (string "||"))
    spaces
    case op of
        "||" -> BinaryExpr lhs TokenType.OR <$> orExpr
        _    -> return lhs

andExpr :: Parser Expression
andExpr = do
    lhs <- equalityExpr <* spaces
    op  <- option "" (string "&&")
    spaces
    case op of
        "&&" -> BinaryExpr lhs TokenType.AND <$> andExpr
        _    -> return lhs

equalityExpr :: Parser Expression
equalityExpr = do
    lhs <- relationExpr <* spaces
    op  <- option "" (try (string "==") <|> try (string "!="))
    spaces
    case op of
        "==" -> BinaryExpr lhs TokenType.EQUALS <$> equalityExpr
        "!=" -> BinaryExpr lhs TokenType.NEQUALS <$> equalityExpr
        _    -> return lhs

relationExpr :: Parser Expression
relationExpr = do
    lhs <- additionExpr <* spaces
    op  <- option
        ""
        (try (string "<=") <|> try (string ">=") <|> try (string "<") <|> try
            (string ">")
        )
    spaces
    case op of
        "<=" -> BinaryExpr lhs TokenType.LESS_EQUALS <$> relationExpr
        ">=" -> BinaryExpr lhs TokenType.GREATER_EQUALS <$> relationExpr
        "<"  -> BinaryExpr lhs TokenType.LESS <$> relationExpr
        ">"  -> BinaryExpr lhs TokenType.GREATER <$> relationExpr
        _    -> return lhs

additionExpr :: Parser Expression
additionExpr = do
    lhs <- multiplicationExpr <* spaces
    op  <- option ' ' (try (oneOf "+-"))
    spaces
    case op of
        '+' -> BinaryExpr lhs TokenType.PLUS <$> additionExpr
        '-' -> BinaryExpr lhs TokenType.MINUS <$> additionExpr
        _   -> return lhs

multiplicationExpr :: Parser Expression
multiplicationExpr = do
    lhs <- unaryExpr <* spaces
    op  <- option ' ' (try (oneOf "*/"))
    spaces
    case op of
        '*' -> BinaryExpr lhs TokenType.MUL <$> multiplicationExpr
        '/' -> BinaryExpr lhs TokenType.DIV <$> multiplicationExpr
        _   -> return lhs

unaryExpr :: Parser Expression
unaryExpr = do
    spaces
    op <- option ' ' (try (oneOf "!-"))
    spaces
    case op of
        '!' -> UnaryExpr TokenType.EXCL_MARK <$> unaryExpr
        '-' -> UnaryExpr TokenType.MINUS <$> unaryExpr
        _   -> try groupingExpr <|> callExpr

callExpr :: Parser Expression
callExpr = CallExpr <$> getCall

getCall :: Parser [Call]
getCall = do
    base <- optionMaybe (try getIdentifier)
    spaces
    case base of
        Nothing -> do
            b <- baseExpr
            return [BaseCall b]
        Just idf -> do
            next <- try anyChar <* spaces
            case next of
                '.' -> do
                    rest <- getCall <* spaces
                    return (IdfCall idf : rest)
                '(' -> do
                    args <- getArgs <* spaces <* char ')' <* spaces
                    rest <- getCall <* spaces
                    return (FnCall idf args : rest)
                _ -> return []

baseExpr :: Parser Expression
baseExpr =
    BaseExpr
        <$> try (many1 digit)
        <|> BaseExpr
        <$> try (string "true")
        <|> BaseExpr
        <$> try (string "false")
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
    comma <- optionMaybe $ char ','
    case comma of
        Nothing -> return [par]
        Just x  -> parameters <?> "expecting parameter"

var :: Parser (String, String)
var = do
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

-- parses one or more spaces 




oneSpaces :: Parser ()
oneSpaces = space >> spaces
