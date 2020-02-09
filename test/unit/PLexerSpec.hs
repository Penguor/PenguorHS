module PLexerSpec where

import           Test.Hspec
import qualified PLexer
import           Parser.TokenType              as TokenType


spec =
    describe "PLexer.tokenize"
        $ it "returns a list of Penguor tokens from a string"
        $ do
              PLexer.tokenize "fn"
                  `shouldBe` [ PLexer.Token "fn" TokenType.FN
                             , PLexer.Token "" TokenType.EOF
                             ]
              PLexer.tokenize "null"
                  `shouldBe` [ PLexer.Token "null" TokenType.NULL
                             , PLexer.Token "" TokenType.EOF
                             ]
            