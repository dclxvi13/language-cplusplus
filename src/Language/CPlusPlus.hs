module Language.CPlusPlus where

import Language.CPlusPlus.Token
import Language.CPlusPlus.Lexer
import Language.CPlusPlus.Comment
import Language.CPlusPlus.PP

type AST = ([Comment], [L Token], [L Token])

tokenizeFile :: String -> IO [L Token]
tokenizeFile path = do
  str <- readFile path
  return $ lexer str

getAST :: [L Token] -> IO AST
getAST tokens = do
  let (comments, ts) = spanComments tokens
  --print comments
  --print ts
  let (pps, ts') = spanPP ts
  return (comments, pps, ts')
