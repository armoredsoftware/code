{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

import VChanUtil
import Packet
import Control.Concurrent
import Data.Binary
import qualified Data.ByteString.Lazy as BS
main = do logger <- createLogger
          ctrlMgr <- createMgrChan_Client logger 1 

                        -- 1/2 second delay
          let loop = do threadDelay(10000* 50)
                        
                        putStrLn "Enter text to send as a Chat packet to Dom 1"
                        input <-getLine
                        print $ BS.length $ encode (Chat (show 0) (show 1) input)
                        print $ encode (Chat (show 0) (show 1) input)
                        sendPacket logger ctrlMgr (encode (Chat (show 0) (show 1) input))
                       
                       -- sendString logger ctrlMgr input
                        -- is there something to be read?
                        size <- dataReady ctrlMgr
                        if size > 0 then
                          do response <- checkMessage logger ctrlMgr size
                             putStrLn ("Received from 1: "++ (show response))
                             loop
                        else
                          loop
           in loop
          destroyLogger logger
{-
getDomId :: IO Int

printf :: String -> IO (CInt)

createLogger :: IO (Ptr XenToolLogger)

destroyLogger :: Ptr XenToolLogger -> IO()

createMgrCtrl :: Ptr XenToolLogger -> IO (Ptr LibXenVChan)

createSrvCtrl :: Ptr XenToolLogger -> Int-> IO (Ptr LibXenVChan)

createClientCtrl :: Ptr XenToolLogger -> Int -> Int -> IO (Ptr LibXenVChan)

ctrlWait :: Ptr LibXenVChan -> IO Int

dataReady :: Ptr LibXenVChan -> IO Int

checkResponse :: Ptr XenToolLogger -> Ptr LibXenVChan -> IO(Int)

checkMessage :: Ptr XenToolLogger -> Ptr LibXenVChan -> IO(String)
-}
