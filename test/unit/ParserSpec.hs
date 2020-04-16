{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import Data.Text

import qualified Parser


spec = do
    describe "program" $ do
        let program = parse Parser.program ""
        it "can parse several declarations" $ do
            program "system STest {} container CTest {} datatype DTest {}"
                `shouldParse` [ Parser.System "STest" "" (Parser.Block [])
                              , Parser.Container "CTest" "" (Parser.Block [])
                              , Parser.Datatype "DTest" "" (Parser.Block [])
                              ]

        describe "declaration" $ do
            let declaration = parse Parser.declaration ""
            it "can execute the system parser" $ do
                declaration "system Test {}"
                    `shouldParse` Parser.System "Test" "" (Parser.Block [])
            it "can execute the container parser" $ do
                declaration "container Test {}"
                    `shouldParse` Parser.Container "Test" "" (Parser.Block [])
            it "can execute the datatype parser" $ do
                declaration "datatype Test {}"
                    `shouldParse` Parser.Datatype "Test" "" (Parser.Block [])
            it "can execute the variable parser" $ do
                declaration "var int test;" `shouldParse` Parser.Var "int" "test"
            it "can execute the function parser" $ do
                declaration "fn int Test() {}" `shouldParse` Parser.Function
                    "int"
                    "Test"
                    []
                    (Parser.Block [])
            it "can execute the library parser" $ do
                declaration "library Test {}"
                    `shouldParse` Parser.Library "Test" (Parser.Block [])
            it "can execute the statement parser" $ do
                declaration "#include Core" `shouldParse` Parser.Stmt
                    (Parser.PPStmt (Parser.Include "Core"))

            describe "statement" $ do
                let statement = parse Parser.statement ""
                it "can execute the preprocessor statement parser" $ do
                    statement "#safety 2" `shouldParse` Parser.PPStmt (Parser.Safety 2)
                it "can execute the if statement parser" $ do
                    pendingWith "to be implemented"

