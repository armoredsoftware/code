{-# LANGUAGE ScopedTypeVariables #-}

-- vchan library
import VChanUtil

import Demo1Utils

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
    do id<- getDomId
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
         


{- Think of an appraisal as the three step process we've talked about:
   1)  Send a request.
   2)  Receive a quote.
   3)  Evaluate.

   For a shallow embedding, each step should be it's own function, for clarity.
   Note that we use the Either monad to propogate errors.
-}
{-
spawnAttestation :: TMVar Shared -> IO ()
spawnAttestation m = atomically $
  do req <- getRequest m
     sq <- mkSignedQuote req
     void . swapTMVar m $ Attestation sq
  where getRequest :: TMVar Shared -> STM Request
        getRequest m =
            do v <- readTMVar m
               case v of
                 Appraisal req -> return req
                 otherwise -> retry
-}
