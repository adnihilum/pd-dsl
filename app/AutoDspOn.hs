module AutoDspOn
  ( autoDspOn
  ) where

import Control.Monad
import PdDSL

autoDspOn :: (PdAsm str m, HasObjectIndexState s m Int) => m ()
autoDspOn = void $ msg "\\; pd dsp 1" $ del 1000 $ loadbang
