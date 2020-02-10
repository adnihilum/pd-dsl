{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Interpretator.PdCompile
  ( PdCompile
  , runPdCompile
  , ObjectIndexState(..)
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import DSL.Types
import Data.List
import Data.String
import DSL.ObjectIndexState

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
  tell $ "#" <> chunkType <> " " <> (mconcat $ intersperse " " params) <> ";\r\n"

data ObjectIndexState =
  ObjectIndexState
    { _objectIndex :: Int
    }

makeFieldsNoPrefix ''ObjectIndexState

instance HasObjectIndexGlobal ObjectIndexState Int where
  objectIndexGlobal = objectIndex