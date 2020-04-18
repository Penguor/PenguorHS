{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Token
    ( Tok(..)
    , TType(..)
    , getByType
    )
where

import           Data.Text                      ( Text )
import           Data.Void

import           Text.Megaparsec

type Parser = Parsec Void [Tok]

data Tok = Tok { typ :: TType, pos :: SourcePos }
    deriving(Show, Eq, Ord)

data TType = HASHTAG
    | FROM | INCLUDE | SAFETY
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
    | VAR
    | NUM {content :: Text}
    | STRING {content :: Text}
    | IDF {content :: Text}
    | TRUE | FALSE
    | FN
    | CONTAINER | SYSTEM | DATATYPE
    | LIBRARY
    | IF | ELIF | ELSE
    | FOR | WHILE | DO
    | SWITCH | CASE | DEFAULT
    | EOF
    | OTHER
    deriving(Show, Eq, Ord)


instance Stream [Tok] where
    type Token [Tok] = Tok
    type Tokens [Tok] = [Tok]

getByType :: TType -> Parser Tok
getByType t = do
    tk <- anySingle
    if t == typ tk then return tk else fail $ "expecting" ++ show t



