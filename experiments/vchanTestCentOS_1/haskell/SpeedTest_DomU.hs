{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}

import VChanUtil
import Data.Time
import qualified Data.ByteString as BS
import Foreign


main = do logger <- createLogger
          id <- getDomId
          putStrLn ("My Dom Id: "++show id)         
          ctrlMgr <- createMgrChan_Srv logger

          loop ctrlMgr
          start <- getCurrentTime
          print start
          run_getter logger 0 0 start ctrlMgr
          destroyLogger logger
            where loop=(\chan -> do size<- dataReady chan
                                    if size > 0 then
                                      putStrLn "Got first element"
                                    else
                                       loop chan)
          
-- #########################################################################

run_getter :: Ptr VChanUtil.XenToolLogger -> Word64 -> Word64 ->
              UTCTime -> Ptr VChanUtil.LibXenVChan-> IO ()
run_getter logger !total !x !start_t !c
  | x >= print_amt = do print_speed start_t total
                        run_getter logger total 0 start_t c
  | otherwise      = do size<- dataReady c
                        if size > 0 then do
                          block <- readChunkedMessageByteString logger c 
                          let !sz = fromIntegral $ BS.length block
                          run_getter logger (total + sz) (x + sz) start_t c
                        else
                          run_getter logger total x start_t c
 where
  print_amt = 128 * 1024 * 1024

-- #########################################################################

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

