module Demo2V1 where

import System.Random

{--
generateNonce :: Int 
generateNonce = 

genereateNonceHelper :: IO Int
generateNonceHelper = getStdRandom((-9000), (9000))
--}
rollDice :: IO Int
rollDice = getStdRandom (randomR ((minBound ::Int),maxBound :: Int))

dosomethin :: Int -> Int
dosomethin x = 2*x

{--
toByteString
--}
