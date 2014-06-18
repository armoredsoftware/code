{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables#-}
{-# LANGUAGE BangPatterns #-}

import Packet
import VChanUtil
import Control.Concurrent
import Control.Monad
import Data.Binary
import Data.Time
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Foreign

createPacket:: String -> IO Packet
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



prompt:: IO(Ptr LibXenVChan, Bool)
prompt = do
          var <- loop
          case var of
            (x,True) -> do  chan<- server_init x
                            return (chan, True)
            (x,False) -> do chan<- client_init x
                            return (chan, False)
           where loop = do putStrLn "Which Domain ID would you like to communicate with?"
                           input <- getLine
                           case reads input of
                             [(id,_)] -> server id
                             _     -> do putStrLn "Error: Please Enter a Number."
                                         loop
                 server id= do putStrLn "Are you the client (0) or server (1)?"
                               input <- getLine
                               case reads input of
                                 [(server,_)] -> return (id , (server == 1))
                                 _     -> do putStrLn "Error: Please Enter a Number."
                                             server id

justReceive chan =forever $ do
                      size <- dataReady chan
                      if size > 0 then
                        do response::Packet <- receive chan       
                           putStrLn ("Received from MGR: "++ (show response))
                           putStr("\n")
                      else
                           putStr "" 

justSend chan = forever $do
                   putStrLn "Which Packet Would you like to send?"
                   putStrLn "0 - Comm, 1-Chat, 2-Attest"
                   packetType <- getLine
                   packet <- createPacket packetType
                   putStrLn $ show packet
                   send chan packet



main = do 
          id <- getDomId
          putStrLn ("Got my Id: "++show id)         
          (chan,server) <- prompt
          case server of
             False -> justSend chan
             True -> justReceive chan
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
                          do response <- readChunkedMessageString logger ctrlMgr 
                         -- do response <- checkMessage logger ctrlMgr size
                             --putStrLn ("Haskell Received: "++(show (length response)))
                             putStrLn ("Received from MGR: "++ (show response))
                             putStr("\n")
                             loop
                        else
                          loop
           in loop
 -}
