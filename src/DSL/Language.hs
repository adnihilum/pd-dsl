{-# LANGUAGE DuplicateRecordFields #-}

module DSL.Language where

import DSL.Types
import Data.String
import Control.Lens

obj :: (PdAsm str m, HasObjIndexState m) => [str] -> m Int
obj args = do
  object $ ["obj", "0", "0"] ++ args
  incObjIndex -- TODO:  increment indexes on all object not just on 'obj'

--TODO:  we should illiminate possobilities of connecting an outlet to an outlet
connect ::
     forall str m p. (PdAsm str m)
  => Node -> PortW -> Node -> PortW -> m ()
connect from fromP to toP = object ["connect", "0", "0", showP from, showP fromP, showP to, showP toP]
  where
    showP :: (HasIdx x Int) => x -> str
    showP = fromString . show . (^. idx)

data PlusWInlets =
  PlusWInlets
    {
    }

plusW :: (PdAsm str m, HasObjIndexState m) => Node -> Node -> m Node
plusW a b = do
  idx <- obj ["+~"]
  let outlet = Node idx (InletSet2W (PortW 0) (PortW 1)) (OutletSet1W $ PortW 0)
  connect a (a ^? outlets . out1) outlet (outlet ^. inlets . in1)
  connect b (a ^? outlets . out1) outlet (outlet ^. inlets . in1)
  return outlet

oscW :: (PdAsm str m, HasObjIndexState m) => Int -> m Node
oscW freq = do
  idx <- obj ["osc~"]
  return $ Node idx InletSetNil (OutletSet1W (PortW 0))

dacW :: (PdAsm str m, HasObjIndexState m) => Node -> Node -> m Node
dacW left right = do
  idx <- obj ["dac~"]
  let node = Node idx (InletSet2W (PortW 0) (PortW 1)) OutletSetNil
  connect left node
  connect right node
  return node

test :: (PdAsm str m, HasObjIndexState m) => m ()
test = do
  oscA <- oscW 480
  oscB <- oscW 640
  plus <- plusW oscA oscB
  dacW plus plus
  return ()
