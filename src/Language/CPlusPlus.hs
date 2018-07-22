module Language.CPlusPlus where

import Language.CPlusPlus.Token
import Language.CPlusPlus.Lexer (lexer)
import Language.CPlusPlus.Comment
import Language.CPlusPlus.PP

type AST = ([Comment], [CppToken], [CppToken])

tokenizeFile :: String -> IO [CppToken]
tokenizeFile path = do
  str <- readFile path
  return $ lexer str

getAST :: [CppToken] -> IO AST
getAST tokens = do
  let (comments, ts) = spanComments tokens
  --print comments
  --print ts
  let (pps, ts') = spanPP ts
  return (comments, pps, ts')
