module ExpressionTest where

import Test.HUnit

import Language.CPlusPlus.Parser
import Language.CPlusPlus.Lexer (lexer)
--import Language.CPlusPlus.Token

testLiteralExpression =
  TestCase $ do
    let testString = ""
        tokens = lexer testString
        res = testParse primaryExpression tokens

    undefined
