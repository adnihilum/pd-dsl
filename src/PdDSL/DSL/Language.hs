{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PdDSL.DSL.Language where

import Control.Lens
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.String
import GHC.TypeLits
import PdDSL.DSL.Initializers
import PdDSL.DSL.ObjectIndexState
import PdDSL.DSL.Types

obj ::
     ( PdAsm str m
     , HasObjectIndexState s m Int
     , InletSetInitializer ins
     , OutletSetInitializer outs
     )
  => [str]
  -> m (m (Node ins outs))
obj args = do
  object $ ["obj", "0", "0"] ++ args
  idx <- incObjIndex -- TODO:  increment indexes on all object not just on 'obj'
  let node = nodeInit idx
  return $ return node

class Connectable a b where
  connect :: (PdAsm str m) => m a -> m b -> m ()

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
  => m pO
  -> m pI
  -> m ()
connect'' from to = do
  from' <- from
  to' <- to
  object
    [ "connect"
    , show' $ from' ^!! nodeIdx
    , show' $ from' ^!! portIdx
    , show' $ to' ^!! nodeIdx
    , show' $ to' ^!! portIdx
    ]

infixl 8 ^!

(^!) :: (Monad m) => (m a) -> Getter a b -> m b
ma ^! b = (^!! b) <$> ma

{-# INLINE (^!) #-}
infixl 8 ^!!

a ^!! b = fromMaybe undefined (a ^? b)

{-# INLINE (^!!) #-}
infixl 7 ~>

a ~> b = connect a b

{-# INLINE (~>) #-}
op2iw1ow :: str -> Op2iw1ow str s ao1 ao2 m
op2iw1ow opName a b = do
  node <- obj [opName]
  a ~> node ^! in1
  b ~> node ^! in2
  node

type Op2iw1ow str s ao1 ao2 m
   = ( PdAsm str m
     , HasObjectIndexState s m Int
     , Connectable ao1 PortIW
     , Connectable ao2 PortIW
     ) =>
       m ao1 -> m ao2 -> m (Node InletSet2W OutletSet1W)

-- operators on wave
-- TODO: create infix aliases for those operators
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

oscWC ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => Float
  -> m (Node InletSet1S OutletSet1W)
oscWC freq = oscW $ float2S freq

oscW ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable freq PortIS)
  => m freq
  -> m (Node InletSet1S OutletSet1W)
oscW freq = do
  node <- obj ["osc~"]
  freq ~> node ^! in1
  node

lopW ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable signal PortIW)
  => Float
  -> m signal
  -> m (Node InletSet1W OutletSet1W)
lopW cutoffFreq input = do
  node <- obj ["lop~", show' cutoffFreq]
  input ~> node ^! in1
  node

clipW ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable signal PortIW)
  => Float
  -> Float
  -> m signal
  -> m (Node InletSet1W OutletSet1W)
clipW minLevel maxLevel input = do
  node <- obj ["clip~", show' minLevel, show' maxLevel]
  input ~> node ^! in1
  node

dacW ::
     ( PdAsm str m
     , HasObjectIndexState s m Int
     , Connectable left PortIW
     , Connectable right PortIW
     )
  => m left
  -> m right
  -> m (Node InletSet2W OutletSetNil)
dacW left right = do
  node <- obj ["dac~"]
  left ~> node ^! in1
  right ~> node ^! in2
  node

vlineW ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable control PortIS)
  => m control
  -> m (Node InletSet1S OutletSet1W)
vlineW control = do
  node <- obj ["vline~"]
  control ~> node ^! in1
  node

repeatFloat ::
     ( KnownNat n
     , PdAsm str m
     , HasObjectIndexState s m Int
     , Connectable input PortIS
     )
  => Proxy n
  -> m input
  -> m (Node InletSet1S (OutletSetNS n))
repeatFloat proxy input = do
  let len = natVal proxy
  node <- obj $ ["trigger"] <> (const "float" <$> [0 .. len])
  input ~> node ^! in1
  node

del ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable argIn PortIS)
  => Float
  -> m argIn
  -> m (Node InletSet1S OutletSet1S)
del delayMs bang = do
  node <- obj ["del", show' delayMs, "1", "msec"]
  bang ~> node ^! in1
  node

loadbang ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => m (Node InletSetNil OutletSet1S)
loadbang = join $ obj ["loadbang"]

msg ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable argIn PortIS)
  => str --NOTE:  looks like we have to choose an absolute type here, in order to do stringy stuff
  -> m argIn
  -> m (Node InletSet1S OutletSet1S)
msg messageData input = do
  object $ ["msg", "0", "0", messageData] --TODO: add escaping of ';'
  idx <- incObjIndex
  let node = return $ nodeInit idx
  input ~> node ^! in1
  node

var :: (PdAsm String m, HasObjectIndexState s m Int) => m a -> m (m a)
var ma = return <$> ma

float2S ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => Float
  -> m (Node InletSet1S OutletSet1S)
float2S value = msg (fromString $ show $ value) $ loadbang

-- tools
show' :: (Show a, IsString str) => a -> str
show' x = fromString $ show x
