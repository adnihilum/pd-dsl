{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PdDSL.Interpretator.PdCompile
  ( PdCompile
  , runPdCompile
  , ObjectIndexState(..)
  , compile
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.String
import PdDSL.DSL.ObjectIndexState
import PdDSL.DSL.Types

newtype (Monoid str, IsString str) =>
        PdCompile str a =
  PdCompile
    { runPdCompile :: StateT ObjectIndexState (Writer str) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter str
           , MonadState ObjectIndexState
           )

instance (Monoid str, IsString str) => PdAsm str (PdCompile str) where
  object = genRecord "X"
  frame = genRecord "N"
  array = genRecord "A"

genRecord :: (IsString str, Monoid str) => str -> [str] -> PdCompile str ()
genRecord chunkType params =
  tell $
  "#" <> chunkType <> " " <> (mconcat $ intersperse " " params) <> ";\r\n"

data ObjectIndexState =
  ObjectIndexState
    { _objectIndex :: Int
    }

makeFieldsNoPrefix ''ObjectIndexState

instance HasObjectIndexGlobal ObjectIndexState Int where
  objectIndexGlobal = objectIndex

compile :: (IsString str, Monoid str) => PdCompile str () -> str
compile graph' = snd result
  where
    graph = frame ["canvas", "0", "0", "100", "100", "10"] >> graph'
    result =
      runWriter $ (runStateT $ runPdCompile $ graph) (ObjectIndexState (-1))
