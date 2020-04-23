{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text

import qualified Parser                        as P
import           Parser.TokenType


spec = describe "program" $ do
    let program = parse P.program ""
    it "can parse several declarations"
        $ program "system STest {} container CTest {} datatype DTest {}"
        `shouldParse` P.Program
                          [ P.System "STest" "" (P.Block [])
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
        describe "dtypeDec" $ do
            let dtypeDec = parse P.dtypeDec ""
            it "can parse datatype declarations"
                $             dtypeDec "datatype Test < Super {}"
                `shouldParse` P.Datatype "Test" "Super" (P.Block [])
        describe "parent" $ do
            let parent = parse P.parent ""
            it "can parse parents" $ parent "< Super" `shouldParse` "Super"
        it "can execute the variable parser"
            $             declaration "var int test;"
            `shouldParse` P.Var "int" "test"
        describe "varDec" $ do
            let varDec = parse P.varDec ""
            it "can parse variable declarations"
                $             varDec "var bool test;"
                `shouldParse` P.Var "bool" "test"
        it "can execute the function parser"
            $             declaration "fn int Test() {}"
            `shouldParse` P.Function "int" "Test" [] (P.Block [])
        describe "function" $ do
            let functionDec = parse P.functionDec ""
            it "can parse function declarations"
                $ functionDec "fn void transform(int x, int y, int z) {}"
                `shouldParse` P.Function
                                  "void"
                                  "transform"
                                  [("int", "x"), ("int", "y"), ("int", "z")]
                                  (P.Block [])
            describe "parameters" $ do
                let parameters = parse P.parameters ""
                it "can parse one parameter"
                    $             parameters "string a"
                    `shouldParse` [("string", "a")]
                it "can parse multiple parameters"
                    $             parameters "string a, int b, bool cd"
                    `shouldParse` [ ("string", "a")
                                  , ("int"   , "b")
                                  , ("bool"  , "cd")
                                  ]
                context "when provided with invalid input" $ do
                    it "fails on trailing commas"
                        $              parameters
                        `shouldFailOn` "bool correct,"
                    it "fails when parameter name is missing"
                        $              parameters
                        `shouldFailOn` "bool correct, string"
                    it "fails when name or type is missing"
                        $              parameters
                        `shouldFailOn` "bool, string test"
                describe "var" $ do
                    let var = parse P.var ""
                    it "parses a single parameter"
                        $             var "string a"
                        `shouldParse` ("string", "a")
        it "can execute the library parser"
            $             declaration "library Test {}"
            `shouldParse` P.Library "Test" (P.Block [])
        describe "libDec" $ do
            let libDec = parse P.libDec ""
            it "can parse library declarations"
                $             libDec "library Test {}"
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
                $             statement "if(true) {i=1;}"
                `shouldParse` P.IfStmt
                                  (P.CallExpr [P.IdfCall "true"])
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfCall "i"])
                                            (P.CallExpr
                                                [P.BaseCall (P.BaseExpr "1")]
                                            )
                                        )
                                  ]
                                  []
                                  []
            it "can execute the while statement parser"
                $             statement "while (!active) { result = a + b;}"
                `shouldParse` P.WhileStmt
                                  (P.UnaryExpr
                                      EXCL_MARK
                                      (P.CallExpr [P.IdfCall "active"])
                                  )
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfCall "result"])
                                            (P.BinaryExpr
                                                (P.CallExpr [P.IdfCall "a"])
                                                PLUS
                                                (P.CallExpr [P.IdfCall "b"])
                                            )
                                        )
                                  ]
            it "can execute the for statement parser"
                $ statement "for(pos : positions){result = result + pos;}"
                `shouldParse` P.ForStmt
                                  "pos"
                                  (P.CallExpr [P.IdfCall "positions"])
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfCall "result"])
                                            (P.BinaryExpr
                                                (P.CallExpr [P.IdfCall "result"]
                                                )
                                                PLUS
                                                (P.CallExpr [P.IdfCall "pos"])
                                            )
                                        )
                                  ]
            it "can execute the do statement parser"
                $             statement "do{a = a + 1;} while(true);"
                `shouldParse` P.DoStmt
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfCall "a"])
                                            (P.BinaryExpr
                                                (P.CallExpr [P.IdfCall "a"])
                                                PLUS
                                                (P.CallExpr
                                                    [ P.BaseCall
                                                          (P.BaseExpr "1")
                                                    ]
                                                )
                                            )
                                        )
                                  ]
                                  (P.CallExpr [P.IdfCall "true"])
            it "can execute the switch statement parser"
                $             statement
                                  "\
\switch(test) \
\{ \
\    case 1: \
\    case 2: \
\        a = a + 1; \
\    default: \
\        a = 2; \
\}"
                `shouldParse` P.SwitchStmt
                                  "test"
                                  [ P.CaseStmt
                                      (P.CallExpr [P.BaseCall (P.BaseExpr "1")])
                                      []
                                  , P.CaseStmt
                                      (P.CallExpr [P.BaseCall (P.BaseExpr "2")])
                                      [ P.ExprStmt
                                            (P.AssignExpr
                                                (P.CallExpr [P.IdfCall "a"])
                                                (P.BinaryExpr
                                                    (P.CallExpr [P.IdfCall "a"])
                                                    PLUS
                                                    (P.CallExpr
                                                        [ P.BaseCall
                                                              (P.BaseExpr "1")
                                                        ]
                                                    )
                                                )
                                            )
                                      ]
                                  ]
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfCall "a"])
                                            (P.CallExpr
                                                [P.BaseCall (P.BaseExpr "2")]
                                            )
                                        )
                                  ]
