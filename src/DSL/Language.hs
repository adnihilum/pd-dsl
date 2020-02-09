{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DSL.Language where

import Control.Lens
import DSL.Initializers
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
  => PortOW
  -> PortIW
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
  where
    showP = fromString . show

infixl 8 ^!

a ^! b = fromMaybe undefined (a ^? b)

{-# INLINE (^!) #-}
plusW ::
     (PdAsm str m, HasObjIndexState m)
  => PortOW
  -> PortOW
  -> m (Node InletSet2W OutletSet1W)
plusW a b = do
  idx <- obj ["+~"]
  let outlet = nodeInit idx
  connect a (outlet ^. in1)
  connect b (outlet ^. in1)
  return outlet

oscW ::
     (PdAsm str m, HasObjIndexState m)
  => Int
  -> m (Node InletSetNil OutletSet1W)
oscW freq = do
  idx <- obj ["osc~"]
  return $ nodeInit idx

dacW ::
     (PdAsm str m, HasObjIndexState m)
  => PortOW
  -> PortOW
  -> m (Node InletSet2W OutletSetNil)
dacW left right = do
  idx <- obj ["dac~"]
  let node = nodeInit idx
  connect left (node ^! in1)
  connect right (node ^! in2)
  return node

test :: (PdAsm str m, HasObjIndexState m) => m ()
test = do
  oscA <- oscW 480
  oscB <- oscW 640
  plus <- plusW (oscA ^! out1) (oscB ^! out1)
  dacW (plus ^! out1) (plus ^! out1)
  return ()
