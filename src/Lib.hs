module Lib
  ( someFunc
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import DSL.Language
import DSL.ObjectIndexState
import DSL.Types
import Data.List
import Data.String
import Interpretator.PdCompile

someFunc :: IO ()
someFunc = do
  let result =
        runWriter $ (runStateT $ runPdCompile $ graph) (ObjectIndexState (-1)) -- TODO: move monad construction into PdCompile module
  putStrLn $ snd $ result
  where
    graph :: (PdAsm String m, HasObjectIndexState s m Int) => m ()
    graph = do
      frame ["canvas", "0", "0", "100", "100", "10"]
      autoDspOn
      signal <- busySignal
      output <- volume 0.1 signal
      dacW output output
      return ()

volume ::
     (PdAsm str m, HasObjectIndexState s m Int, Connectable signal PortIW)
  => Float
  -> signal
  -> m PortOW
volume volValue input = do
  volC <- floatConst volValue
  m <- multW volC input
  return $ m ^! out1

busySignal ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => m (Node InletSet2W OutletSet1W) --TODO: add custom node constructor (with imbedded inlets and outlets)
busySignal = do
  signal <- signalG
  cutOff <- cutOffG
  multW signal cutOff
  where
    signalG = do
      osc1 <- oscW 480
      osc2 <- oscW 620
      plusW osc1 osc2
    cutOffG = do
      osc <- oscW 2
      oscAmp <- floatConst 10000
      multW osc oscAmp >>= clipW 0 1 >>= lopW 100

autoDspOn :: (PdAsm str m, HasObjectIndexState s m Int) => m ()
autoDspOn = void $ loadbang >>= del 1000 >>= msg "\\; pd dsp 1"

floatConst :: (PdAsm str m, HasObjectIndexState s m Int) => Float -> m PortOS
floatConst value = do
  lb <- loadbang
  m <- msg (fromString $ show $ value) $ lb ^! out1
  return $ m ^! out1
