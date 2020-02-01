
module Lib
  ( someFunc
  ) where

import Control.Monad.Writer
import Data.String
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class (Monad m) =>
      PdAsm m
  where
  object :: (IsString str) => [str] -> m ()

instance (Monad m, MonadWriter str m, IsString str) => PdAsm m where
  object :: (IsString str) => [str] -> m()
  object params = tell $ "X" <> (mconcat $ intersperse " " params) <> ";\r\n"

