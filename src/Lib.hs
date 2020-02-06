module Lib
  ( someFunc
  ) where

import Control.Monad.Identity
import Control.Monad.Writer
import DSL.Types
import Data.List
import Data.String
import Interpretator.PdCompile

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  let result = runWriter $ runPdCompile $ mf
  putStrLn $ snd $ result
  where
    mf :: (PdAsm String m) => m ()
    mf = do
      object ["aaaaaaaa", "123"]
      object ["zzzzzzz", "321"]
      object ["1", "2", "3"]


{--
newtype (IsString str, Monad m, MonadWriter str m) =>
     DotCompile str m a =
  DotCompile
    { runDotCompile :: m a
    } deriving (Functor, Applicative, Monad, MonadWriter str)
--}
