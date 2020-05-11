{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Parser.Token
    ( Tok(..)
    , TType(..)
    , getByType
    , getType
    , content
    , PStream(..)
    , TokenPos(..)
    )
where

import qualified Data.Text                     as T
                                         hiding ( Text )
import           Data.Text                      ( Text )
import           Data.Void
import           Data.Proxy
import qualified Data.List                     as DL
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec

type Parser = Parsec Void PStream


data TType = HASHTAG
    | FROM | INCLUDE | SAFETY -- pp directives -- ? maybe make them statements?
    | PUBLIC | PRIVATE | PROTECTED | RESTRICTED -- access modifiers
    | STATIC | DYNAMIC -- non-access modifiers
    | ABSTRACT | CONST -- non-access modifiers
    | LPAREN | RPAREN -- ()
    | LBRACE | RBRACE -- {}
    | LBRACK | RBRACK -- []
    | PLUS | MINUS -- basic math operations
    | MUL | DIV | PERCENT -- basic math operations
    | DPLUS | DMINUS -- ++ and --
    | GREATER | LESS
    | GREATER_EQUALS | LESS_EQUALS
    | EQUALS | NEQUALS
    | AND | OR | XOR | NOT -- logical operations
    | BW_AND | BW_OR | BW_XOR | BW_NOT -- bitwise operations
    | BS_LEFT | BS_RIGHT -- bitshift operations
    | ASSIGN -- assignment
    | ADD_ASSIGN | SUB_ASSIGN
    | MUL_ASSIGN | DIV_ASSIGN | PERCENT_ASSIGN
    | BW_AND_ASSIGN | BW_OR_ASSIGN | BW_XOR_ASSIGN
    | BS_LEFT_ASSIGN | BS_RIGHT_ASSIGN
    | NULL
    | COLON | SEMICOLON | DOT | COMMA | EXCL_MARK
    | NUM
    | STRING
    | IDF
    | TRUE | FALSE
    | CONTAINER | SYSTEM | DATATYPE
    | LIBRARY
    | IF | ELIF | ELSE
    | FOR | WHILE | DO
    | SWITCH | CASE | DEFAULT
    | EOF
    | OTHER
    deriving(Show, Eq, Ord)

data Tok = Tok { typ :: TType, pos :: TokenPos, txt :: Text }
    deriving(Eq, Ord)

data TokenPos = TokenPos {startPos :: SourcePos, endPos :: SourcePos, tokLength :: Int}
    deriving(Eq, Ord)

instance Show Tok where
    show a = "(Token type: " ++ show (typ a) ++ if T.unpack (txt a) == ""
        then ")"
        else ", value: " ++ T.unpack (txt a) ++ ")"

data PStream = PStream { streamInput :: Text, streamTokens :: [Tok]}

-- ! implement better version, currently just copied from https://markkarpov.com/tutorial/megaparsec.html


instance Stream PStream where
    type Token PStream = Tok
    type Tokens PStream = [Tok]
    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (PStream _ []) = Nothing
    take1_ (PStream input (t : ts)) =
        Just (t, PStream (T.drop (tokLength (pos t)) input) ts)
    takeN_ n (PStream input ts)
        | n <= 0
        = Just ([], PStream input ts)
        | null ts
        = Nothing
        | otherwise
        = let (x, ts') = splitAt n ts
          in  case NE.nonEmpty x of
                  Nothing -> Just (x, PStream input ts')
                  Just nex ->
                      Just (x, PStream (T.drop (tokensLength pxy nex) input) ts)
    takeWhile_ f (PStream input s) =
        let (x, ts) = DL.span f s
        in  case NE.nonEmpty x of
                Nothing -> (x, PStream input ts)
                Just nex ->
                    (x, PStream (T.drop (tokensLength pxy nex) input) ts)
    showTokens Proxy = show
    tokensLength Proxy ts = sum (tokLength . pos <$> ts)
    reachOffset o PosState {..} =
        ( prefix ++ restOfLine
        , PosState
            { pstateInput      = PStream { streamInput  = T.pack postStr
                                         , streamTokens = post
                                         }
            , pstateOffset     = max pstateOffset o
            , pstateSourcePos  = newSourcePos
            , pstateTabWidth   = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix       = if sameLine then pstateLinePrefix ++ preStr else preStr
        sameLine     = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos = case post of
            []      -> pstateSourcePos
            (x : _) -> startPos (pos x)
        (pre, post) = splitAt (o - pstateOffset) (streamTokens pstateInput)
        (preStr, postStr) =
            splitAt tokensConsumed (T.unpack (streamInput pstateInput))
        tokensConsumed = case NE.nonEmpty pre of
            Nothing    -> 0
            Just nePre -> tokensLength pxy nePre
        restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy PStream
pxy = Proxy

getByType :: TType -> Parser Tok
getByType t = do
    tk <- anySingle
    if t == typ tk then return tk else fail $ "expecting " ++ show t

getType :: TType -> Parser TType
getType t = do
    tk <- anySingle
    if t == typ tk then return (typ tk) else fail $ "expecting " ++ show t

content :: Text -> Parser Tok
content a = do
    tk <- choice [getByType NUM, getByType STRING, getByType IDF]
    if a == txt tk then return tk else fail $ "expecting " ++ show a
