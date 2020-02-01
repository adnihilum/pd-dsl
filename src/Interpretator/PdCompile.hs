{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpretator.PdCompile where

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
  object params = tell $ "X " <> (mconcat $ intersperse " " params) <> ";\r\n"
