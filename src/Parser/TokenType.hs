module Parser.TokenType where

data TokenType = HASHTAG
    | FROM | INCLUDE | NF
    | LPAREN | RPAREN
    | LBRACE | RBRACE
    | LBRACK | RBRACK
    | PLUS | MINUS | MUL | DIV
    | GREATER | LESS
    | GREATER_EQUALS | LESS_EQUALS
    | EQUALS | NEQUALS
    | AND | OR | NOT
    | ASSIGN
    | NULL
    | COLON | SEMICOLON | DOT | COMMA | EXCL_MARK
    | VAR | NUM | STRING | IDF
    | TRUE | FALSE
    | FN
    | COMPONENT | SYSTEM | DATATYPE
    | IF | ELIF | ELSE
    | FOR | WHILE | DO
    | SWITCH | CASE | DEFAULT
    | EOF
    | OTHER
    deriving(Show, Eq)
