{-# LANGUAGE RankNTypes #-}
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

class Connectable a b where
  connect :: (PdAsm str m) => a -> b -> m ()

instance ( HasOut1' o1 a
         , Connectable a b
         , HasNodeIdx b Int
         , HasPortIdx b Int
         , HasNodeIdx a Int
         , HasPortIdx a Int
         ) =>
         Connectable (Node i o1) b where
  connect a = connect'' (a ^! out1)

instance Connectable PortOW PortIW where
  connect = connect''

instance Connectable PortOS PortIS where
  connect = connect''

instance Connectable PortOS PortIW where
  connect = connect''

connect'' ::
     ( PdAsm str m
     , HasNodeIdx pO Int
     , HasPortIdx pO Int
     , HasNodeIdx pI Int
     , HasPortIdx pI Int
     )
  => pO
  -> pI
  -> m ()
connect'' from to =
  object
    [ "connect"
    , show' $ from ^! nodeIdx
    , show' $ from ^! portIdx
    , show' $ to ^! nodeIdx
    , show' $ to ^! portIdx
    ]

infixl 8 ^!

a ^! b = fromMaybe undefined (a ^? b)

{-# INLINE (^!) #-}
infixl 7 ~>

a ~> b = connect a b

{-# INLINE (~>) #-}
op2iw1ow :: str -> Op2iw1ow str s ao1 ao2 m
op2iw1ow opName a b = do
  idx <- obj [opName]
  let node = nodeInit idx
  a ~> node ^! in1
  b ~> node ^! in2
  return node

type Op2iw1ow str s ao1 ao2 m
   = ( PdAsm str m
     , HasObjectIndexState s m Int
     , Connectable ao1 PortIW
     , Connectable ao2 PortIW
     ) =>
       ao1 -> ao2 -> m (Node InletSet2W OutletSet1W)

-- operators on wave
plusW :: Op2iw1ow str ao1 ao2 s m
plusW = op2iw1ow "+~"

minusW :: Op2iw1ow str ao1 ao2 s m
minusW = op2iw1ow "-~"

multW :: Op2iw1ow str ao1 ao2 s m
multW = op2iw1ow "*~"

divW :: Op2iw1ow str ao1 ao2 s m
divW = op2iw1ow "/~"

maxW :: Op2iw1ow str ao1 ao2 s m
maxW = op2iw1ow "max~"

minW :: Op2iw1ow str ao1 ao2 s m
minW = op2iw1ow "min~"

oscW ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => Float
  -> m (Node InletSetNil OutletSet1W)
oscW freq = do
  idx <- obj ["osc~", show' freq]
  return $ nodeInit idx

lopW ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable signal PortIW)
  => Float
  -> signal
  -> m (Node InletSet1W OutletSet1W)
lopW cutoffFreq input = do
  idx <- obj ["lop~", show' cutoffFreq]
  let node = nodeInit idx
  input ~> node ^! in1
  return $ node

clipW ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable signal PortIW)
  => Float
  -> Float
  -> signal
  -> m (Node InletSet1W OutletSet1W)
clipW minLevel maxLevel input = do
  idx <- obj ["clip~", show' minLevel, show' maxLevel]
  let node = nodeInit idx
  input ~> node ^! in1
  return $ node

dacW ::
     ( PdAsm str m
     , HasObjectIndexState s m Int
     , Connectable left PortIW
     , Connectable right PortIW
     )
  => left
  -> right
  -> m (Node InletSet2W OutletSetNil)
dacW left right = do
  idx <- obj ["dac~"]
  let node = nodeInit idx
  left ~> node ^! in1
  right ~> node ^! in2
  return node

del ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable argIn PortIS)
  => Float
  -> argIn
  -> m (Node InletSet1S OutletSet1S)
del delayMs bang = do
  idx <- obj ["del", show' delayMs, "1", "msec"]
  let node = nodeInit idx
  bang ~> node ^! in1
  return node

loadbang ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => m (Node InletSetNil OutletSet1S)
loadbang = nodeInit <$> obj ["loadbang"]

msg ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable argIn PortIS)
  => str --NOTE:  looks like we have to choose an absolute type here, in order to do stringy stuff
  -> argIn
  -> m (Node InletSet1S OutletSet1S)
msg messageData input = do
  object $ ["msg", "0", "0", messageData] --TODO: add escaping of ';'
  idx <- incObjIndex
  let node = nodeInit idx
  input ~> node ^! in1
  return node

-- tools
show' :: (Show a, IsString str) => a -> str
show' x = fromString $ show x
