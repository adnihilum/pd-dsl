{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpretator.PdCompile
  ( PdCompile
  , runPdCompile
  ) where

import Control.Monad.Writer
import DSL.Types
import Data.List
import Data.String

newtype (Monoid str, IsString str) =>
        PdCompile str a =
  PdCompile
    { runPdCompile :: Writer str a
    }
  deriving (Functor, Applicative, Monad, MonadWriter str)

instance (Monoid str, IsString str) => PdAsm str (PdCompile str) where
  object = genRecord "X"
  frame = genRecord "N"
  array = genRecord "A"

genRecord :: (IsString str, Monoid str) => str -> [str] -> PdCompile str ()
genRecord chunkType params =
  tell $ chunkType <> " " <> (mconcat $ intersperse " " params) <> ";\r\n"
