{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DSL.Language where

import Control.Lens
import DSL.Types
import Data.Maybe (fromMaybe)
import Data.String

obj :: (PdAsm str m, HasObjIndexState m) => [str] -> m Int
obj args = do
  object $ ["obj", "0", "0"] ++ args
  incObjIndex -- TODO:  increment indexes on all object not just on 'obj'

--TODO:  we should illiminate possobilities of connecting an outlet to an outlet
connect ::
     forall str m p. (PdAsm str m)
  => PortW
  -> PortW
  -> m ()
connect from to =
  object
    [ "connect"
    , "0"
    , "0"
    , showP $ from ^! nodeIdx
    , showP $ from ^! portIdx
    , showP $ to ^! nodeIdx
    , showP $ to ^! portIdx
    ]
    --showP :: (HasIdx x Int) => x -> str
  where
    showP = fromString . show

infixl 8 ^!

a ^! b = fromMaybe undefined $ (a ^? b)

{-# INLINE (^!) #-}
plusW ::
     (PdAsm str m, HasObjIndexState m)
  => PortW
  -> PortW
  -> m (Node InletSet2W OutletSet1W)
plusW a b = do
  idx <- obj ["+~"]
  let outlet =
        Node
          idx
          (InletSet2W (PortW idx 0) (PortW idx 1))
          (OutletSet1W $ PortW idx 0)
  connect a (outlet ^. inlets . in1)
  connect b (outlet ^. inlets . in1)
  return outlet

oscW ::
     (PdAsm str m, HasObjIndexState m)
  => Int
  -> m (Node InletSetNil OutletSet1W)
oscW freq = do
  idx <- obj ["osc~"]
  return $ Node idx InletSetNil (OutletSet1W (PortW idx 0))

dacW ::
     (PdAsm str m, HasObjIndexState m)
  => PortW
  -> PortW
  -> m (Node InletSet2W OutletSetNil)
dacW left right = do
  idx <- obj ["dac~"]
  let node = Node idx (InletSet2W (PortW idx 0) (PortW idx 1)) OutletSetNil
  connect left (node ^! inlets . in1)
  connect right (node ^! inlets . in2)
  return node

test :: (PdAsm str m, HasObjIndexState m) => m ()
test = do
  oscA <- oscW 480
  oscB <- oscW 640
  plus <- plusW (oscA ^! outlets . out1) (oscB ^! outlets . out1)
  dacW (plus ^! outlets . out1) (plus ^! outlets . out1)
  return ()
