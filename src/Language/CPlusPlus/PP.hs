module Language.CPlusPlus.PP where

import           Language.CPlusPlus.Lexer

data PPStructure =
  PPStructure [PPDirective]

data PPDirective =
  PP

spanPP :: [L Token] -> ([L Token], [L Token])
spanPP ts = spanPP_ ts [] []

spanPP_ [] pps rest = (pps, rest)
spanPP_ (L _ EOL:ts) pps rest = spanPP_ ts pps rest
spanPP_ ((L p PP_Define):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Define) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_If):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_If) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Ifdef):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Ifdef) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Ifndef):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Ifndef) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Elif):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Elif) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Else):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Else) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Endif):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Endif) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Include):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Include) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Undef):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Undef) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Line):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Line) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Error):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Error) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ ((L p PP_Pragma):ts) pps rest = spanPP_ rs (pps ++ ps ++ [r]) rest
  where
    (ps, rrs) = break isEOL $ (L p PP_Pragma) : ts
    isEOL = \(L _ t) -> t == EOL
    r:rs = rrs
spanPP_ (t:ts) pps rest = spanPP_ ts pps $ rest ++ [t]
