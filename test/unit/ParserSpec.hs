{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text

import           Build
import qualified Parser                        as P
import           Parser.Token



spec = describe "program" $ do
    let program a = buildFromSource P.program a
    it "can parse multiple declarations"
        $ program "system STest {} container CTest {} datatype DTest {}"
        `shouldParse` P.Program
                          [ P.System (P.IdfExpr "STest") Nothing (P.Block [])
                          , P.Container (P.IdfExpr "CTest") Nothing (P.Block [])
                          , P.Datatype (P.IdfExpr "DTest") Nothing (P.Block [])
                          ]

    describe "declaration" $ do
        let declaration = buildFromSource P.declaration
        it "can execute the system parser"
            $             declaration "system Test {}"
            `shouldParse` P.System (P.IdfExpr "Test") Nothing (P.Block [])
        describe "sysDec" $ do
            let sysDec = buildFromSource P.sysDec
            it "can parse systems"
                $             sysDec "system Test < Super {}"
                `shouldParse` P.System (P.IdfExpr "Test")
                                       (Just (P.IdfExpr "Super"))
                                       (P.Block [])
        it "can execute the container parser"
            $             declaration "container Test {}"
            `shouldParse` P.Container (P.IdfExpr "Test") Nothing (P.Block [])
        describe "contDec" $ do
            let contDec = buildFromSource P.contDec
            it "can parse containers"
                $             contDec "container Test < Super {}"
                `shouldParse` P.Container (P.IdfExpr "Test")
                                          (Just (P.IdfExpr "Super"))
                                          (P.Block [])
        it "can execute the datatype parser"
            $             declaration "datatype Test {}"
            `shouldParse` P.Datatype (P.IdfExpr "Test") Nothing (P.Block [])
        describe "dtypeDec" $ do
            let dtypeDec = parse P.dtypeDec ""
            it "can parse datatype declarations"
                $             dtypeDec "datatype Test < Super {}"
                `shouldParse` P.Datatype (P.IdfExpr "Test")
                                         (Just (P.IdfExpr "Super"))
                                         (P.Block [])
        describe "parent" $ do
            let parent = parse P.parent ""
            it "can parse parents" $ parent "< Super" `shouldParse` Just
                (P.IdfExpr "Super")
        it "can execute the variable parser"
            $             declaration "var int test;"
            `shouldParse` P.Var (P.IdfExpr "int") (P.IdfExpr "test")
        describe "varDec" $ do
            let varDec = parse P.varDec ""
            it "can parse variable declarations"
                $             varDec "var bool test;"
                `shouldParse` P.Var (P.IdfExpr "bool") (P.IdfExpr "test")
        it "can execute the function parser"
            $             declaration "fn int test() {}"
            `shouldParse` P.Function (P.IdfExpr "int")
                                     (P.IdfExpr "test")
                                     []
                                     (P.Block [])
        describe "function" $ do
            let functionDec = parse P.functionDec ""
            it "can parse function declarations"
                $ functionDec "fn void transform(int x, int y, int z) {}"
                `shouldParse` P.Function
                                  (P.IdfExpr "void")
                                  (P.IdfExpr "transform")
                                  [ ((P.IdfExpr "int"), (P.IdfExpr "x"))
                                  , ((P.IdfExpr "int"), (P.IdfExpr "y"))
                                  , ((P.IdfExpr "int"), (P.IdfExpr "z"))
                                  ]
                                  (P.Block [])
            describe "parameters" $ do
                let parameters = parse P.parameters ""
                it "can parse one parameter"
                    $             parameters "string a"
                    `shouldParse` [((P.IdfExpr "string"), (P.IdfExpr "a"))]
                it "can parse multiple parameters"
                    $             parameters "string a, int b, bool cd"
                    `shouldParse` [ ((P.IdfExpr "string"), (P.IdfExpr "a"))
                                  , ((P.IdfExpr "int")   , (P.IdfExpr "b"))
                                  , ((P.IdfExpr "bool")  , (P.IdfExpr "cd"))
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
                    `shouldParse` ((P.IdfExpr "string"), (P.IdfExpr "a"))
        it "can execute the library parser"
            $             declaration "library Test {}"
            `shouldParse` P.Library (P.IdfExpr "Test") (P.Block [])
        describe "libDec" $ do
            let libDec = parse P.libDec ""
            it "can parse library declarations"
                $             libDec "library Test {}"
                `shouldParse` P.Library (P.IdfExpr "Test") (P.Block [])
        it "can execute the statement parser"
            $             declaration "#include Core"
            `shouldParse` P.Stmt (P.PPStmt (P.Include (P.IdfExpr "Core")))
        describe "statement" $ do
            let statement = parse P.statement ""
            it "can execute the preprocessor statement parser"
                $             statement "#safety 2"
                `shouldParse` P.PPStmt (P.Safety 2)
            describe "preProcessorStmt" $ do
                let preProcessorStmt = parse P.preProcessorStmt ""
                it "can parse preprocessor statements"
                    $             preProcessorStmt "#include Math"
                    `shouldParse` P.PPStmt (P.Include (P.IdfExpr "Math"))
                describe "ppDirective" $ do
                    let ppDirective = parse P.ppDirective ""
                    it "can parse preprocessor directives"
                        $             ppDirective "include Test"
                        `shouldParse` P.Include (P.IdfExpr "Test")
                    describe "include" $ do
                        let include = parse P.include ""
                        it "can parse library includes"
                            $             include "include Test"
                            `shouldParse` P.Include (P.IdfExpr "Test")
                    describe "fromIncl" $ do
                        let fromIncl = parse P.fromIncl ""
                        it "can parse from includes"
                            $ fromIncl "from CoreLib include HelloWorld"
                            `shouldParse` P.FromIncl
                                              (P.IdfExpr "CoreLib")
                                              (P.IdfExpr "HelloWorld")
                    describe "safety" $ do
                        let safety = parse P.safety ""
                        it "can parse safety levels"
                            $             safety "safety 1"
                            `shouldParse` P.Safety 1
                        context "when provided with invalid input"
                            $ it "fails when provided with digits > 2"
                            $ do
                                  safety `shouldFailOn` "safety 3"
                                  safety `shouldFailOn` "safety 4"
                                  safety `shouldFailOn` "safety 5"
                                  safety `shouldFailOn` "safety 6"
                                  safety `shouldFailOn` "safety 7"
                                  safety `shouldFailOn` "safety 8"
                                  safety `shouldFailOn` "safety 9"
            it "can execute the if statement parser"
                $             statement "if(true) {i=1;}"
                `shouldParse` P.IfStmt
                                  (P.CallExpr [P.BaseExpr TRUE] [])
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfExpr "i"] [])
                                            (P.CallExpr [P.NumExpr 1] [])
                                        )
                                  ]
                                  []
                                  []
            it "can execute the while statement parser"
                $             statement "while (!active) { result = a + b;}"
                `shouldParse` P.WhileStmt
                                  (P.UnaryExpr
                                      EXCL_MARK
                                      (P.CallExpr [P.IdfExpr "active"] [])
                                  )
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfExpr "result"] [])
                                            (P.BinaryExpr
                                                (P.CallExpr [P.IdfExpr "a"] [])
                                                PLUS
                                                (P.CallExpr [P.IdfExpr "b"] [])
                                            )
                                        )
                                  ]
            it "can execute the for statement parser"
                $ statement "for(Pos pos : positions){result = result + pos;}"
                `shouldParse` P.ForStmt
                                  (P.IdfExpr "pos", P.IdfExpr "pos")
                                  (P.CallExpr [P.IdfExpr "positions"] [])
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfExpr "result"] [])
                                            (P.BinaryExpr
                                                (P.CallExpr
                                                    [P.IdfExpr "result"]
                                                    []
                                                )
                                                PLUS
                                                (P.CallExpr [P.IdfExpr "pos"] []
                                                )
                                            )
                                        )
                                  ]
            it "can execute the do statement parser"
                $             statement "do{a = a + 1;} while(true);"
                `shouldParse` P.DoStmt
                                  [ P.ExprStmt
                                        (P.AssignExpr
                                            (P.CallExpr [P.IdfExpr "a"] [])
                                            (P.BinaryExpr
                                                (P.CallExpr [P.IdfExpr "a"] [])
                                                PLUS
                                                (P.CallExpr [P.NumExpr 1] [])
                                            )
                                        )
                                  ]
                                  (P.CallExpr [P.BaseExpr TRUE] [])
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
                                  (P.IdfExpr "test")
                                  [ P.CaseStmt (P.CallExpr [P.NumExpr 1] []) []
                                  , P.CaseStmt
                                      (P.CallExpr [P.NumExpr 2] [])
                                      [ P.ExprStmt
                                            (P.AssignExpr
                                                (P.CallExpr [P.IdfExpr "a"] [])
                                                (P.BinaryExpr
                                                    (P.CallExpr
                                                        [P.IdfExpr "a"]
                                                        []
                                                    )
                                                    PLUS
                                                    (P.CallExpr [P.NumExpr 1] []
                                                    )
                                                )
                                            )
                                      ]
                                  ]
                                  (Just
                                      [ P.ExprStmt
                                            (P.AssignExpr
                                                (P.CallExpr [P.IdfExpr "a"] [])
                                                (P.CallExpr [P.NumExpr 2] [])
                                            )
                                      ]
                                  )
            it "can execute the expression statement parser"
                $             statement "a = b;"
                `shouldParse` P.ExprStmt
                                  (P.AssignExpr
                                      (P.CallExpr [P.IdfExpr "a"] [])
                                      (P.CallExpr [P.IdfExpr "b"] [])
                                  )
            describe "expression" $ do
                let expression = parse P.expression ""
                it "can parse expressions"
                    $             expression "a = b;"
                    `shouldParse` P.AssignExpr
                                      (P.CallExpr [P.IdfExpr "a"] [])
                                      (P.CallExpr [P.IdfExpr "b"] [])
                it "can parse assign expressions"
                    $             expression "name = \"Peter\";"
                    `shouldParse` P.AssignExpr
                                      (P.CallExpr [P.IdfExpr "name"] [])
                                      (P.CallExpr [P.StringExpr "Peter"] [])
                it "can parse or expressions"
                    $             expression "true || false;"
                    `shouldParse` P.BinaryExpr
                                      (P.CallExpr [P.BaseExpr TRUE] [])
                                      OR
                                      (P.CallExpr [P.BaseExpr FALSE] [])
                it "can parse and expressions"
                    $             expression "true && isEntity;" -- ! inspect why some expressions need a semicolon to parse

                    `shouldParse` P.BinaryExpr
                                      (P.CallExpr [P.BaseExpr TRUE] [])
                                      AND
                                      (P.CallExpr [P.IdfExpr "isEntity"] [])
                it "can parse equality expressions"
                    $             expression "testVar == 54"
                    `shouldParse` P.BinaryExpr
                                      (P.CallExpr [P.IdfExpr "testVar"] [])
                                      EQUALS
                                      (P.CallExpr [P.NumExpr 54] [])
                it "can parse relation expressions"
                    $             expression "a1var <= 23.3"
                    `shouldParse` P.BinaryExpr
                                      (P.CallExpr [P.IdfExpr "a1var"] [])
                                      LESS_EQUALS
                                      (P.CallExpr [P.NumExpr 233] [])
                it "can parse addition expressions"
                    $             expression "i + 24"
                    `shouldParse` P.BinaryExpr
                                      (P.CallExpr [P.IdfExpr "i"] [])
                                      PLUS
                                      (P.CallExpr [P.NumExpr 24] [])
                it "can parse multiplication expressions"
                    $             expression "t_var / 2"
                    `shouldParse` P.BinaryExpr
                                      (P.CallExpr [P.IdfExpr "t_var"] [])
                                      DIV
                                      (P.CallExpr [P.NumExpr 2] [])
                it "can parse unary expressions"
                    $             expression "-2"
                    `shouldParse` P.UnaryExpr MINUS
                                              (P.CallExpr [P.NumExpr 2] [])
                it "can parse call expressions"
                    $             expression "test;"
                    `shouldParse` P.CallExpr [P.IdfExpr "test"] []
