module DSL.Language where

import DSL.Types
import Data.String

obj :: (PdAsm str m, HasObjIndexState m) => [str] -> m Int
obj args = do
  object $ ["obj", "0", "0"] ++ args
  incObjIndex

connect ::
     forall str m p. (PdAsm str m, HasPortNumber p)
  => p
  -> p
  -> m ()
connect from to = object ["connect", "0", "0", showP from, showP to]
  where
    showP = fromString . show . getPort

plusW :: (PdAsm str m, HasObjIndexState m) => Node -> Node -> m Node
plusW a b = do
  idx <- obj ["+~"]
  let outlet = Node idx
  connect a outlet
  connect b outlet
  return outlet

oscW :: (PdAsm str m, HasObjIndexState m) => Int -> m Node
oscW freq = Node <$> obj ["osc~"]

dacW :: (PdAsm str m, HasObjIndexState m) => Node -> Node -> m ()
dacW left right = do
  node <- Node <$> obj ["osc~"]
  connect left node
  connect right node

test :: (PdAsm str m, HasObjIndexState m) => m ()
test = do
  oscA <- oscW 480
  oscB <- oscW 640
  plus <- plusW oscA oscB
  dacW plus plus
