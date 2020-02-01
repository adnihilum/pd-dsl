module DSL.Types where

class (Monad m) =>
      PdAsm str m
  where
  object :: [str] -> m ()
