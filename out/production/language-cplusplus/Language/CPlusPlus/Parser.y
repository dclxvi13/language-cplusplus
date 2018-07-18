{
module Language.CPlusPlus.Parser where

import qualified Language.CPlusPlus.Lexer as L
}

%name       parse
%tokentype  { L.Token }
%error      { parseError }