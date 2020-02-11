module Lib
  ( someFunc
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import DSL.Language
import DSL.Types
import Data.List
import Data.String
import Interpretator.PdCompile

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  let result =
        runWriter $ (runStateT $ runPdCompile $ test) (ObjectIndexState (-1)) -- TODO: move monad construction into PdCompile module
  putStrLn $ snd $ result
  where
    mf :: (PdAsm String m) => m ()
    mf = do
      object ["aaaaaaaa", "123"]
      object ["zzzzzzz", "321"]
      object ["1", "2", "3"]

test :: (Monoid str, IsString str) => PdCompile str ()
test = do
  frame ["canvas", "0", "0", "100", "100", "10"] -- #N canvas 0 35 1920 1021 10; --TODO: add this line inside runPdCompile or somewhere around there
  oscA <- oscW 480
  oscB <- oscW 640
  plus <- plusW (oscA ^! out1) (oscB ^! out1)
  dacW (plus ^! out1) (plus ^! out1)
  return ()
