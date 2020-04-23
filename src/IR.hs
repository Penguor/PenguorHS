{-# LANGUAGE OverloadedStrings #-}

module IR where

import           Parser
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Parser.Data

data IR = IR [IRDec]

data IRDec = OBJECT Text Content | Empty

data Content = Ins [IRInstruction] | EmptyCt

data IRInstruction = Call Address

astToir :: Program -> IR
astToir p = IR [Empty]

-- todo: implement super
declarationIR :: Declaration -> [IRDec]
declarationIR (System n _ b) = do
    let content = blockIR b
    [OBJECT n content]
declarationIR (Container n _ b) = do
    let content = blockIR b
    [OBJECT n content]
declarationIR (Datatype n _ b) = do
    let content = blockIR b
    [OBJECT n content]

blockIR :: Block -> Content
blockIR = a
