{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PdDSL.DSL.Types where

import Control.Lens
import Control.Monad
import Data.String
import GHC.TypeLits

class (IsString str, Monad m) =>
      PdAsm str m
  | m -> str
  where
  object :: [str] -> m ()
  frame :: [str] -> m ()
  array :: [str] -> m ()

--TODO:  add port types for messages (and also for control messaging: it's a little bit different thing in the PD wonder land)
data PortIS =
  PortIS
    { _nodeIdx :: Int
    , _portIdx :: Int
    }

makeFieldsNoPrefix ''PortIS

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

data PortOS =
  PortOS
    { _nodeIdx :: Int
    , _portIdx :: Int
    }

makeFieldsNoPrefix ''PortOS

data InletSetNil =
  InletSetNil

data InletSet1W =
  InletSet1W
    { _in1' :: PortIW
    }

makeFieldsNoPrefix ''InletSet1W

data InletSet2W =
  InletSet2W
    { _in1' :: PortIW
    , _in2' :: PortIW
    }

makeFieldsNoPrefix ''InletSet2W

data InletSet1S =
  InletSet1S
    { _in1' :: PortIS
    }

makeFieldsNoPrefix ''InletSet1S

data OutletSetNil =
  OutletSetNil

data OutletSet1W =
  OutletSet1W
    { _out1' :: PortOW
    }

makeFieldsNoPrefix ''OutletSet1W

data OutletSet1S =
  OutletSet1S
    { _out1' :: PortOS
    }

makeFieldsNoPrefix ''OutletSet1S

newtype OutletSetNS (n :: Nat) =
  OutletSetNS
    { _outs :: [PortOS]
    }

makeFieldsNoPrefix ''OutletSetNS

data Node ins outs =
  Node
    { _idx :: Int
    , _inlets :: ins
    , _outlets :: outs
    }

makeFieldsNoPrefix ''Node

out1 :: (HasOut1' os o) => Lens' (Node is os) o
out1 = outlets . out1'

in1 :: (HasIn1' is o) => Lens' (Node is os) o
in1 = inlets . in1'

in2 :: (HasIn2' is o) => Lens' (Node is os) o
in2 = inlets . in2'
