{-# LANGUAGE ScopedTypeVariables #-}

--vchan library
import VChanUtil

import Demo1Utils



prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to Appraise?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop


-- The fun stuff
main :: IO ()
main = 
    do  let req = mkRequest [0..7]
        id<- getDomId 
        putStrLn $ "Domain id: "++(show id)
        other <- prompt
        chan <- client_init other
        putStrLn $ "Sending: "++(show req)
        send chan req 
        ctrlWait chan
        res :: Shared<- receive chan
        putStrLn $ "Received: "++(show res)
        print $ evaluate req res
          
        putStrLn "End"
  

{- Think of an appraisal as the three step process we've talked about:
   1)  Send a request.
   2)  Receive a quote.
   3)  Evaluate.

   For a shallow embedding, each step should be it's own function, for clarity.
   Note that we use the Either monad to propogate errors.
-}
{-
spawnAppraisal :: TMVar Shared -> IO ()
spawnAppraisal m =
  let req = mkRequest [0..7] in
    do atomically $ do cond <- tryPutTMVar m $ Appraisal req
                       when (not cond) . throwSTM $ 
                         ErrorCall "Target not ready for request."
       sq <- atomically $ getSignedQuote m
       atomically $ do result <- evaluate req sq
                       void . swapTMVar m $ Result result  
  where getSignedQuote :: TMVar Shared -> STM Quote
        getSignedQuote m  = 
            do v <- readTMVar m
               case v of
                 Attestation sq -> return sq
                 otherwise -> retry
-}
