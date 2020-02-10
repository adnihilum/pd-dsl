{-# LANGUAGE ConstraintKinds #-}
module DSL.ObjectIndexState where

import Control.Lens
import Control.Monad.State

-- class (Num n) => HasObjectIndexState m n | m -> n where
--   incObjIndex :: m n

class HasObjectIndexGlobal s a | s -> a where
    objectIndexGlobal :: Lens' s a

type HasObjectIndexState s m n = (MonadState s m, HasObjectIndexGlobal s n, Num n)
incObjIndex :: (HasObjectIndexState s m n) => m n

incObjIndex = 
        state (objectIndexGlobal <+~ 1)

-- one = 1 + 1.1