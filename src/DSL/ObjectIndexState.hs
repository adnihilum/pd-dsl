{-# LANGUAGE ConstraintKinds #-}

module DSL.ObjectIndexState where

import Control.Lens
import Control.Monad.State

class HasObjectIndexGlobal s a | s -> a where
  objectIndexGlobal :: Lens' s a

type HasObjectIndexState s m n
   = (MonadState s m, HasObjectIndexGlobal s n, Num n)

incObjIndex :: (HasObjectIndexState s m n) => m n
incObjIndex = state (objectIndexGlobal <+~ 1)
