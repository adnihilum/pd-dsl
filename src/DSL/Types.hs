{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DSL.Types where

import Control.Lens
import Control.Monad
import Data.String

class (IsString str, Monad m) =>
      PdAsm str m
  | m -> str
  where
  object :: [str] -> m ()
  frame :: [str] -> m ()
  array :: [str] -> m ()

data PortW =
  PortW
    { _idx :: Int
    }

makeFieldsNoPrefix ''PortW

data InletSet
  = InletSetNil
  | InletSet1W
      { _in1 :: PortW
      }
  | InletSet2W
      { _in1 :: PortW
      , _in2 :: PortW
      }

makeFieldsNoPrefix ''InletSet

data OutletSet
  = OutletSetNil
  | OutletSet1W
      { _out1 :: PortW
      }

makeFieldsNoPrefix ''OutletSet

data Node =
  Node
    { _idx :: Int
    , _inlets :: InletSet
    , _outlets :: OutletSet
    }

makeFieldsNoPrefix ''Node

class HasObjIndexState m where
  incObjIndex :: m Int
