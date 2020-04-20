{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase        #-}

module Parser.Token
    ( Tok(..)
    , TType(..)
    , getByType
    , content
    , PStream(..)
    )
where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Void
import           Data.Proxy
import qualified Data.List                     as DL
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec

type Parser = Parsec Void PStream


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
    | NUM
    | STRING
    | IDF
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

data Tok = Tok { typ :: TType, pos :: SourcePos, txt :: Text }
    deriving(Eq, Ord)

instance Show Tok where
    show a = "(Token type: " ++ show (typ a) ++ if unpack (txt a) == ""
        then ")"
        else ", value: " ++ unpack (txt a) ++ ")"

data PStream = PStream { streamInput :: String, streamTokens :: [Tok]}

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
    take1_ (PStream str (t : ts)) =
        Just (t, PStream (drop (tokensLength' Proxy (t NE.:| [])) str) ts)
    takeN_ n (PStream str s)
        | n <= 0
        = Just ([], PStream str s)
        | null s
        = Nothing
        | otherwise
        = let (x, s') = splitAt n s
          in  case NE.nonEmpty x of
                  Nothing -> Just (x, PStream str s')
                  Just nex ->
                      Just (x, PStream (drop (tokensLength' Proxy nex) str) s')
    takeWhile_ f (PStream str s) =
        let (x, s') = DL.span f s
        in  case NE.nonEmpty x of
                Nothing -> (x, PStream str s')
                Just nex ->
                    (x, PStream (drop (tokensLength' Proxy nex) str) s')
    showTokens Proxy = show
    reachOffset o PosState {..} =
        ( prefix ++ restOfLine
        , PosState
            { pstateInput      = PStream { streamInput  = postStr
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
            (x : _) -> pos x
        (pre, post) = splitAt (o - pstateOffset) (streamTokens pstateInput)
        (preStr, postStr) = splitAt tokensConsumed (streamInput pstateInput)
        tokensConsumed = case NE.nonEmpty pre of
            Nothing    -> 0
            Just nePre -> tokensLength' Proxy nePre
        restOfLine = takeWhile (/= '\n') postStr


tokensLength' :: Proxy PStream -> NonEmpty (Token PStream) -> Int
tokensLength' a b = tokensLength a b


getByType :: TType -> Parser Tok
getByType t = do
    tk <- anySingle
    if t == typ tk then return tk else fail $ "expecting" ++ show t

content :: Text -> Parser Tok
content a = do
    tk <- choice [getByType NUM, getByType STRING, getByType IDF]
    if a == txt tk then return tk else fail $ "expecting" ++ show a
