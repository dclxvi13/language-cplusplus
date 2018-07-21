module Language.CPlusPlus.Base where

import Language.CPlusPlus.AST
import Language.CPlusPlus.Token

import Text.Parsec (between)

angles :: P a -> P a
angles = between (opLess) (opGreater)

brackets :: P a -> P a
brackets = between (leftBracket) (rightBracket)

braces :: P a -> P a
braces = between (leftBrace) (rightBrace)

parens :: P a -> P a
parens = between (leftParen) (rightParen)

