{-# LANGUAGE ScopedTypeVariables #-}

-- vchan library
import VChanUtil

import Demo1Utils
import Data.Maybe
import Control.Monad

prompt:: IO (Int)
--prompt = return 6
prompt= loop
      where loop = do putStrLn "Which Domain ID is the Appraiser?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop

-- The fun stuff
main :: IO ()
main = 
    do id <- getDomId
       let priKey = getPriKey
       putStrLn $ "Domain id: "++(show id)
       appraiser<- prompt
       chan <- server_init appraiser
       ctrlWait chan
       val:: Shared <-receive chan
       putStrLn $ "Attestation Received: "++(show val)
       let sQuote = mkSignedQuote priKey val
       putStrLn $ "Attestation Sending: "++(show sQuote) 
       send chan  sQuote
       ctrlWait chan
       putStr ""
    
 --      close chan
