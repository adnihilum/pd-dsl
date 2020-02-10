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

data PortIW =
  PortIW
    { _nodeIdx :: Int
    , _portIdx :: Int
    }

makeFieldsNoPrefix ''PortIW

data PortOW =
  PortOW
    { _nodeIdx :: Int
    , _portIdx :: Int
    }

makeFieldsNoPrefix ''PortOW

data InletSetNil =
  InletSetNil

data InletSet1W =
  InletSet1W
    { _in1 :: PortIW
    }

makeFieldsNoPrefix ''InletSet1W

data InletSet2W =
  InletSet2W
    { _in1 :: PortIW
    , _in2 :: PortIW
    }

makeFieldsNoPrefix ''InletSet2W

data OutletSetNil =
  OutletSetNil

data OutletSet1W =
  OutletSet1W
    { _out1 :: PortOW
    }

makeFieldsNoPrefix ''OutletSet1W

data Node ins outs =
  Node
    { _idx :: Int
    , _inlets :: ins
    , _outlets :: outs
    }

makeFieldsNoPrefix ''Node

instance (HasOut1 o PortOW) => HasOut1 (Node i o) PortOW where
  out1 = outlets . out1

instance (HasIn1 i PortIW) => HasIn1 (Node i o) PortIW where
  in1 = inlets . in1

instance (HasIn2 i PortIW) => HasIn2 (Node i o) PortIW where
  in2 = inlets . in2

