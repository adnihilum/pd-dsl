module DSL.Types where

import Control.Monad
import Data.String

class (IsString str, Monad m) =>
      PdAsm str m
  | m -> str
  where
  object :: [str] -> m ()
  frame :: [str] -> m ()
  array :: [str] -> m ()

data Node =
  Node Int

class HasObjIndexState m where
  incObjIndex :: m Int

-- TODO:  rewrite using lenses
class HasPortNumber a where
  getPort :: a -> Int

instance HasPortNumber Node where
  getPort (Node a) = a
