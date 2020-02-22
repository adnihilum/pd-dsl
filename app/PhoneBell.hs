{-# LANGUAGE DataKinds #-}

module PhoneBell where

import Control.Lens
import Data.Proxy
import Data.String
import PdDSL

bell pitch = multW (oscWB pitch)

bellenv decay = do
  signal <- var $ vlineW (msg "1 0 0, 0 $1 0" decay)
  multW signal signal

partialground p1 v1 p2 v2 p3 v3 decay =
  bells `multW` (floatConst 0.333) `multW` bellenv decay
  where
    bells = (bell p1 v1) `plusW` (bell p2 v2) `plusW` (bell p3 v3)

bellPrime fundamental' strength' decay = do
  f <- var $ repeatFloat (Proxy :: Proxy 3) fundamental'
  s <- var $ repeatFloat (Proxy :: Proxy 3) strength'
  let out t n = t ^! (outlets . outs . ix n)
  partialground
    (out f 0 `mult` (floatConst 0.501))
    (out s 0 `mult` (floatConst 0.002))
    (out f 1)
    (out s 1 `mult` (floatConst 0.02))
    (out f 2 `mult` (floatConst 0.7))
    (out s 2 `mult` (floatConst 0.001))
    (decay `mult` 1.2)

--helpers
floatConst ::
     (PdAsm str m, HasObjectIndexState s m Int)
  => Float
  -> m (Node InletSet1S OutletSet1S)
floatConst value = msg (fromString $ show $ value) $ loadbang
