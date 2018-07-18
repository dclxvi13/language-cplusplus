module Language.CPlusPlus.Comment
  ( Comment(..)
  , spanComments
  ) where

import           Language.CPlusPlus.Lexer

data Comment = Comment
  { _commentText :: String
  , _commentPos  :: Pos
  } deriving (Show, Eq)

spanComments :: [L Token] -> ([Comment], [L Token])
spanComments ts = spanComments_ (addEOLIfNotExist ts) [] []

spanComments_ [] comments rest = (comments, rest)
spanComments_ (t:ts) comments rest =
  case t of
    L p (TComment s) -> spanComments_ ts (comments ++ [Comment s p]) rest
    L p (TLineComment s) -> spanComments_ ts (comments ++ [Comment s p]) (rest ++ [L p EOL])
    _ -> spanComments_ ts comments (rest ++ [t])

addEOLIfNotExist ts =
  let L (l, _) t = last ts
   in if t == EOL
        then ts
        else ts ++ [L (l + 1, 1) EOL]
