{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
import VChanUtil
import Packet
import Control.Monad
import Control.Concurrent
import Data.Binary
import qualified Data.ByteString.Lazy as BSL

createPacket packetType = case packetType of
                            "0" -> do putStrLn "[SrcId] [DestId]"
                                      input <- getLine
                                      let src = takeWhile ((/=) ' ') input
                                          dest = tail $ dropWhile ((/=) ' ') input
                                        in return (CommRequest src dest)
                            "1" -> do putStrLn "[SrcId] [DestId] [msg]"
                                      input <- getLine
                                      let src = takeWhile((/=) ' ') input
                                          dest = takeWhile ((/=) ' ') $tail $ dropWhile ((/=) ' ') input
                                          msg = tail $dropWhile ((/=) ' ') $ tail $ dropWhile ((/=) ' ') input
                                       in return (Chat src dest msg)
                            "2" -> do putStrLn "[SrcId] [DestId] [attestation Req]"
                                      input <- getLine
                                      let src = takeWhile((/=) ' ') input
                                          dest = takeWhile ((/=) ' ') $tail $ dropWhile ((/=) ' ') input
                                          msg = tail $dropWhile ((/=) ' ') $ tail $ dropWhile ((/=) ' ') input
                                       in return (AttestationRequest src dest msg)


dataBlob = BSL.pack byteList
  where byteList = [0..]++byteList



main = do logger <- createLogger
          ctrlMgr <- createMgrChan_Client logger 1 
     {-     forever $ do
                       space <-getBufferSpace ctrlMgr 
                       if space > 0 then
                          do sendChunkedMessage logger ctrlMgr $ BSL.toStrict $ BSL.take space dataBlob
                             putStr ""
                       else
                           putStr ""
                       --threadDelay(10000*50)
                       -- 
     -} 
          
                        -- 1/2 second delay
          let loop = do threadDelay(10000* 50)
                        --putStrLn "Which Kind of Packet?\n0-CommRequest 1-Chat 2-AttestationRequest" 
                        putStrLn "Send a Chunked message to ID 1" 
                        packetType <-getLine
                       -- packet <- createPacket packetType
                       -- sendPacket logger ctrlMgr packet
                       -- sendPacketString logger ctrlMgr (Chat (show 0) (show 1) input)
                       -- sendString logger ctrlMgr input
                        sendChunkedMessage logger ctrlMgr packetType
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
