module Demo2V1 where

import System.Ramdom

{--
generateNonce :: Int 
generateNonce = 
--}
genereateNonceHelper :: IO Int
generateNonceHelper = getStdRandom((minBound Int), (maxBound Int))

dosomethin :: Int -> Int
dosomethin x = 2*x

{--
toByteString
--}
