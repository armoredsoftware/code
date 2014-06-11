{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}

import Packet
import VChanUtil
import Control.Concurrent
import Data.Binary
import Data.Time
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Foreign

createPacket packetType = case packetType of
                            "0" -> do putStrLn "[SrcId] [DestId]"
                                      input <- getLine
                                      let src = takeWhile ((/=) ' ') input
                                          dest = tail $ dropWhile ((/=) ' ') input
                                        in return (CommRequest src dest)
                            "1" -> do putStrLn "[SrcId] [DestId] [msg]"
                                      input <- getLine
                                      let src = takeWhile((/=) ' ') input
                                          dest = takeWhile ((/=) ' ') $ tail $ dropWhile ((/=) ' ') input
                                          msg = tail $dropWhile ((/=) ' ') $ tail $ dropWhile ((/=) ' ') input
                                       in return (Chat src dest msg)
                            "2" -> do putStrLn "[SrcId] [DestId] [attestation Req]"
                                      input <- getLine
                                      let src = takeWhile((/=) ' ') input
                                          dest = takeWhile ((/=) ' ') $ tail $ dropWhile ((/=) ' ') input
                                          msg = tail $dropWhile ((/=) ' ') $ tail $ dropWhile ((/=) ' ') input
                                       in return (AttestationRequest src dest msg)



main = do logger <- createLogger
          id <- getDomId
          putStrLn ("Got my Id: "++show id)         
          ctrlMgr <- createMgrChan_Srv logger

          loop ctrlMgr
          start <- getCurrentTime
          print start
          run_getter logger 0 0 start ctrlMgr

 {-         
                        -- 1/2 second delay
          let loop = do --threadDelay(10000* 50)

                     {-   putStrLn "Enter text to send as a Chat packet to Dom 1"
                        input <-getLine
                        print $ BSL.length $ encode (Chat (show 0) (show 1) input)
                        print $ encode (Chat (show 0) (show 1) input)
                        
                        --sendPacket logger ctrlMgr (Chat (show 0) (show 1) input)
                        sendString logger ctrlMgr input
                     -}
                        -- is there something to be read?
                        size <- dataReady ctrlMgr
                        if size > 0 then
                         -- do response <- getBlob logger ctrlMgr size
                          do response <- readChunkedMessage logger ctrlMgr 
                         -- do response <- checkMessage logger ctrlMgr size
                             --putStrLn ("Haskell Received: "++(show (length response)))
                             putStrLn ("Received from MGR: "++ (show response))
                             putStr("\n")
                             loop
                        else
                          loop
           in loop
 -}
          destroyLogger logger
            where loop=(\chan -> do size<- dataReady chan
                                    if size > 0 then
                                      putStrLn "Got first element"
                                    else
                                       loop chan)
          
run_getter :: Ptr VChanUtil.XenToolLogger  ->
              Word64 -> Word64 ->
              UTCTime -> Ptr VChanUtil.LibXenVChan->
              IO ()
run_getter logger !total !x !start_t !c
  | x >= print_amt = do print_speed start_t total
                        run_getter logger total 0 start_t c
  | otherwise      = do size<- dataReady c
                        if size > 0 then do
                          block <- getBlob logger c size
                          let !sz = fromIntegral $ BS.length block
                          run_getter logger (total + sz) (x + sz) start_t c
                        else
                          run_getter logger total x start_t c
 where
  print_amt = 128 * 1024 * 1024

print_speed :: UTCTime -> Word64 -> IO ()
print_speed start numBytes = do
  now <- getCurrentTime
  let !diff     = diffUTCTime now start
      !diff_rat = toRational diff :: Rational
      !diff_dbl = fromRational diff_rat :: Float
      !amnt_dbl = fromIntegral numBytes :: Float
  putStrLn $ "Read " ++ show numBytes ++ " in " ++ (show diff)
  putStrLn $ "   " ++ showSpeed (amnt_dbl / diff_dbl) ++ "\n"
 where
  showSpeed x 
    | x < onek  = show x ++ " bps"
    | x < onem  = show (x / onek) ++ " KBps"
    | x < oneg  = show (x / onem) ++ " MBps"
    | otherwise = show (x / oneg) ++ " GBps"
  onek = 1024
  onem = 1024 * 1024
  oneg = 1024 * 1024 * 1024

