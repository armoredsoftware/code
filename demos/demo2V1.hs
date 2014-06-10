module Demo2V1 where

import Data.Binary as Bin
import System.Random

main :: IO ()
main = do
  nonce <- rollHugeDice
  print nonce




rollHugeDice ::IO Int
rollHugeDice = getStdRandom (randomR ((minBound ::Int),maxBound :: Int))
      
dosomethin :: Int -> Int
dosomethin x = 2*x

{--
toByteString
--}
