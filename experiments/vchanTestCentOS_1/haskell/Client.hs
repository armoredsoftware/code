{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

import Packet
import VChanUtil
import Control.Concurrent
import Data.Binary
import qualified Data.ByteString.Lazy as BS

main = do logger <- createLogger
          id <- getDomId
          putStrLn ("Got my Id: "++show id)         
          ctrlMgr <- createMgrChan_Srv logger

                        -- 1/2 second delay
          let loop = do threadDelay(10000* 50)
{-
                        putStrLn "Enter text to send as a Chat packet to Dom 1"
                        input <-getLine
                        print $ BS.length $ encode (Chat (show 0) (show 1) input)
                        print $ encode (Chat (show 0) (show 1) input)
                        
                        --sendPacket logger ctrlMgr (Chat (show 0) (show 1) input)
                        sendString logger ctrlMgr input
  -}                     
                        -- is there something to be read?
                        size <- dataReady ctrlMgr
                        if size > 0 then
                          do response <- checkMessage logger ctrlMgr size
                             --putStrLn ("Haskell Received: "++(show (length response)))
                             putStrLn ("Received from MGR: "++ (show response))
                             loop
                        else
                          loop
           in loop
          destroyLogger logger
          
