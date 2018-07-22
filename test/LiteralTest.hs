module LiteralTest where

import Test.HUnit

import Language.CPlusPlus.Parser
import Language.CPlusPlus.Lexer

testLiteral =
  TestCase $
    let testString = "123"
        tokens = lexer testString
    res <- testParse literal tokens
    undefined
