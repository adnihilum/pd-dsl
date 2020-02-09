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
    { _nodeIdx :: Int
    , _portIdx :: Int
    }

makeFieldsNoPrefix ''PortW

data InletSetNil =
  InletSetNil

data InletSet1W =
  InletSet1W
    { _in1 :: PortW
    }

makeFieldsNoPrefix ''InletSet1W

data InletSet2W =
  InletSet2W
    { _in1 :: PortW
    , _in2 :: PortW
    }

makeFieldsNoPrefix ''InletSet2W

data OutletSetNil =
  OutletSetNil

data OutletSet1W =
  OutletSet1W
    { _out1 :: PortW
    }

makeFieldsNoPrefix ''OutletSet1W

data Node ins outs =
  Node
    { _idx :: Int
    , _inlets :: ins
    , _outlets :: outs
    }

makeFieldsNoPrefix ''Node

class HasObjIndexState m where
  incObjIndex :: m Int
