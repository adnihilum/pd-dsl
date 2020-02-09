module DSL.Initializers where

import DSL.Types

nodeInit ::
     (InletSetInitializer ins, OutletSetInitializer outs)
  => Int
  -> Node ins outs
nodeInit idx = Node idx (inletSetInit idx) (outletSetInit idx)

class InletSetInitializer ins where
  inletSetInit :: Int -> ins

instance InletSetInitializer InletSetNil where
  inletSetInit idx = InletSetNil

instance InletSetInitializer InletSet1W where
  inletSetInit idx = InletSet1W (PortIW idx 0)

instance InletSetInitializer InletSet2W where
  inletSetInit idx = InletSet2W (PortIW idx 0) (PortIW idx 1)

class OutletSetInitializer outs where
  outletSetInit :: Int -> outs

instance OutletSetInitializer OutletSetNil where
  outletSetInit idx = OutletSetNil

instance OutletSetInitializer OutletSet1W where
  outletSetInit idx = OutletSet1W (PortOW idx 0)