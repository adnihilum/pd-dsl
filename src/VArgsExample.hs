module VArgsExample where

test :: IO ()
test = do
  print $ (opr "1" "2" "3" "4" :: [String])
  print $ (oprP "lol" "1" "2" "3" "4" :: [String])
    {-class Test r where
    opr' :: [IO ()] -> r

instance Test (IO ()) where
    opr'  = sequence_ 

instance (Show a, Test r) => Test (a -> r) where
    opr' acc x = opr' (acc ++ [print x])

opr :: (Test r) => r
opr = opr' [] 
-}

{-
class Test a r | r -> a where
  opr' :: [a] -> r

instance Test a [a] where
  opr' acc = acc

instance (Show a, Test a r) => Test a (a -> r) where
  opr' acc x = opr' (acc ++ [x])

opr :: (Test String r) => r
opr = opr' []
-}
class Test a r | r -> a where
  opr' :: [a] -> r

instance Test a [a] where
  opr' acc = acc

instance (Show a, Test a r) => Test a (a -> r) where
  opr' acc x = opr' (acc ++ [x])

opr :: (Test String r) => r
opr = opr' []

oprP :: (Test String r) => String -> r
oprP str = opr' [] --(\l -> intersperse str l)
