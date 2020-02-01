module Lib
  ( someFunc
  ) where

import Control.Monad.Writer
import Data.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class PdAsm m where
  object :: m ()

instance (MonadWriter str m, IsString str) => PdAsm m where
    object = tell "x"
