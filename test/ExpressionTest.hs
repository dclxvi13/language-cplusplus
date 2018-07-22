module ExpressionTest where

import Test.HUnit

import Language.CPlusPlus.Parser
import Language.CPlusPlus.Lexer (lexer)
--import Language.CPlusPlus.Token

testLiteralExpression =
  TestCase $ do
    let testString = "123"
        expected = Right $ LiteralExpression _ $ Literal _ "123"
        res = testParse primaryExpression $ lexer testString

    assertEqual "literal expression" expected res
