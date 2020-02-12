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
      output <- volume 0.1 (signal ^! out1)
      dacW output output
      return ()

volume ::
     (PdAsm str m, HasObjectIndexState s m Int) => Float -> PortOW -> m PortOW
volume volValue input = do
  volC <- floatConst volValue
  m <- multW volC input
  return $ m ^! out1

busySignal ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => m (Node InletSet2W OutletSet1W)
busySignal = do
  signal <- signalG
  cutOff <- cutOffG
  multW signal cutOff
  where
    signalG = do
      osc1 <- oscW 480
      osc2 <- oscW 620
      pl <- plusW (osc1 ^! out1) (osc2 ^! out1)
      return $ pl ^! out1
    cutOffG = do
      osc <- oscW 2
      oscAmp <- floatConst 10000
      oscA <-
        multW (osc ^! out1) oscAmp >>= (\x -> clipW 0 1 (x ^! out1)) >>=
        (\y -> lopW 100 (y ^! out1))
      return $ oscA ^! out1

autoDspOn :: (PdAsm str m, HasObjectIndexState s m Int) => m ()
autoDspOn = do
  lb <- loadbang
  dl <- del 1000 $ lb ^! out1
  msg "\\; pd dsp 1" $ dl ^! out1
  return ()

floatConst :: (PdAsm str m, HasObjectIndexState s m Int) => Float -> m (PortOS)
floatConst value = do
  lb <- loadbang
  m <- msg (fromString $ show $ value) $ lb ^! out1
  return $ m ^! out1
