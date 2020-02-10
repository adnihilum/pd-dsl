{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DSL.Language where

import Control.Lens
import DSL.Initializers
import DSL.ObjectIndexState
import DSL.Types
import Data.Maybe (fromMaybe)
import Data.String

obj :: (PdAsm str m, HasObjectIndexState s m Int) => [str] -> m Int
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
infixl 7 ~>

a ~> b = connect a b

{-# INLINE (~>) #-}
plusW ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => PortOW
  -> PortOW
  -> m (Node InletSet2W OutletSet1W)
plusW a b = do
  idx <- obj ["+~"]
  let outlet = nodeInit idx
  a ~> outlet ^. in1
  b ~> outlet ^. in2
  return outlet

oscW ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => Int
  -> m (Node InletSetNil OutletSet1W)
oscW freq = do
  idx <- obj ["osc~"]
  return $ nodeInit idx

dacW ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => PortOW
  -> PortOW
  -> m (Node InletSet2W OutletSetNil)
dacW left right = do
  idx <- obj ["dac~"]
  let node = nodeInit idx
  left ~> node ^! in1
  right ~> node ^! in2
  return node
