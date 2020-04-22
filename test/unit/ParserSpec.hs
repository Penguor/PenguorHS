{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text

import qualified Parser                        as P


spec = describe "program" $ do
    let program = parse P.program ""
    it "can parse several declarations"
        $ program "system STest {} container CTest {} datatype DTest {}"
        `shouldParse` [ P.System "STest" "" (P.Block [])
                      , P.Container "CTest" "" (P.Block [])
                      , P.Datatype "DTest" "" (P.Block [])
                      ]

    describe "declaration" $ do
        let declaration = parse P.declaration ""
        it "can execute the system parser"
            $             declaration "system Test {}"
            `shouldParse` P.System "Test" "" (P.Block [])
        describe "sysDec" $ do
            let sysDec = parse P.sysDec ""
            it "can parse systems"
                $             sysDec "system Test < Super {}"
                `shouldParse` P.System "Test" "Super" (P.Block [])
        it "can execute the container parser"
            $             declaration "container Test {}"
            `shouldParse` P.Container "Test" "" (P.Block [])
        describe "contDec" $ do
            let contDec = parse P.contDec ""
            it "can parse containers"
                $             contDec "container Test < Super {}"
                `shouldParse` P.Container "Test" "Super" (P.Block [])
        it "can execute the datatype parser"
            $             declaration "datatype Test {}"
            `shouldParse` P.Datatype "Test" "" (P.Block [])
        it "can execute the variable parser"
            $             declaration "var int test;"
            `shouldParse` P.Var "int" "test"
        it "can execute the function parser"
            $             declaration "fn int Test() {}"
            `shouldParse` P.Function "int" "Test" [] (P.Block [])
        it "can execute the library parser"
            $             declaration "library Test {}"
            `shouldParse` P.Library "Test" (P.Block [])
        it "can execute the statement parser"
            $             declaration "#include Core"
            `shouldParse` P.Stmt (P.PPStmt (P.Include "Core"))

        describe "statement" $ do
            let statement = parse P.statement ""
            it "can execute the preprocessor statement parser"
                $             statement "#safety 2"
                `shouldParse` P.PPStmt (P.Safety 2)
            it "can execute the if statement parser"
                $ pendingWith "to be implemented"

