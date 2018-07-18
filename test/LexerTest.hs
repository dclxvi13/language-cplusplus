module LexerTest where

import Language.CPlusPlus

lexFile = do
  let file = "/home/dclxvi13/UnrealEngine/Engine/Source/Runtime/AIModule/Private/AIModule.cpp"
  ts <- tokenizeFile file
  (cs, pps, ts) <- getAST ts
  print $ "cs: " ++ show cs
  print $ "pps: " ++ show pps
  print $ "ts: " ++ show ts