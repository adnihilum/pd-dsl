module BusySignal
  ( graph
  ) where

import AutoDspOn
import Data.List
import Data.String
import PdDSL

graph :: (PdAsm String m, HasObjectIndexState s m Int) => m ()
graph = do
  autoDspOn
  output <- var $ volume 0.1 busySignal
  dacW output output
  return ()

volume ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable signal PortIW)
  => Float
  -> m signal
  -> m (Node InletSet2W OutletSet1W)
volume volValue input = multW (floatConst volValue) input

busySignal ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => m (Node InletSet2W OutletSet1W) --TODO: add custom node constructor (with imbedded inlets and outlets)
busySignal = do
  let signal = plusW (oscW 480) (oscW 620)
  let cutOff = lopW 100 $ clipW 0 1 $ multW (oscW 2) (floatConst 10000)
  multW signal cutOff

floatConst ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => Float
  -> m (Node InletSet1S OutletSet1S)
floatConst value = msg (fromString $ show $ value) $ loadbang
