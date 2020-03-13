module Parser where


import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Char

program :: Parser String
program = declaration

declaration :: Parser String
declaration = sysDec

sysDec :: Parser String
sysDec = return "sysDec"

string :: Parser String
string = do
    satisfy (== '"')
    str <- many text
    satisfy (== '"')
    return str
    where text = satisfy (/= '"')
