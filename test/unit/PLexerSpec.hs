module PLexerSpec where

import           Test.Hspec
import qualified PLexer
import           Parser.TokenType              as TokenType


spec = describe "PLexer.tokenize" $ do
    it "turns a string into a token" $ do
        PLexer.tokenize "fn"
            `shouldBe` [ PLexer.Token "fn" TokenType.FN
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "null"
            `shouldBe` [ PLexer.Token "null" TokenType.NULL
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "system"
            `shouldBe` [ PLexer.Token "system" TokenType.SYSTEM
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "component"
            `shouldBe` [ PLexer.Token "component" TokenType.COMPONENT
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "datatype"
            `shouldBe` [ PLexer.Token "datatype" TokenType.DATATYPE
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "if"
            `shouldBe` [ PLexer.Token "if" TokenType.IF
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "while"
            `shouldBe` [ PLexer.Token "while" TokenType.WHILE
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "for"
            `shouldBe` [ PLexer.Token "for" TokenType.FOR
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "do"
            `shouldBe` [ PLexer.Token "do" TokenType.DO
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "from"
            `shouldBe` [ PLexer.Token "from" TokenType.FROM
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "include"
            `shouldBe` [ PLexer.Token "include" TokenType.INCLUDE
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "var"
            `shouldBe` [ PLexer.Token "var" TokenType.VAR
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "true"
            `shouldBe` [ PLexer.Token "true" TokenType.TRUE
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "false"
            `shouldBe` [ PLexer.Token "false" TokenType.FALSE
                       , PLexer.Token "" TokenType.EOF
                       ]
    it "splits up a string into several tokens" $ do
        PLexer.tokenize "var test"
            `shouldBe` [ PLexer.Token "var" TokenType.VAR
                       , PLexer.Token "test" TokenType.IDF
                       , PLexer.Token "" TokenType.EOF
                       ]
        PLexer.tokenize "\"test\""
            `shouldBe` [ PLexer.Token "test" TokenType.STRING
                       , PLexer.Token "" TokenType.EOF
                       ]
