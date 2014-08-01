{-# LANGUAGE ScopedTypeVariables #-}
import Network.Transport
import VChanUtil
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC



mkTransport id = return Transport
                          { newEndPoint = serverEnd id
                          , closeTransport = closeT
                          }

serverEnd :: Int -> IO(Either (TransportError NewEndPointErrorCode) EndPoint)
serverEnd oid = do vchan <- server_init oid
                   myId <- getDomId

                   let myAddress = EndPointAddress (BSC.pack (show myId))
                       noNewMulticast = return $ Left (TransportError NewMulticastGroupUnsupported "Unsupported Method")
                       noResolveMulticast= \_-> return $ Left (TransportError ResolveMulticastGroupUnsupported "Unsupported Method")

                   return $ Right EndPoint
                                 { Network.Transport.receive = receiveData oid vchan
                                 , address = myAddress
                                 , connect = createConnection
                                 , newMulticastGroup = noNewMulticast
                                 , resolveMulticastGroup = noResolveMulticast
                                 , closeEndPoint = undefined
                                 }

closeT  = undefined
--Unsure about the Connection ID if the client Id is sufficient
receiveData :: Int -> LibXenVChan -> IO Event
receiveData clientId chan = do ready<- dataReady chan
                               if ready < 0 then
                                    do ctrlWait chan
                                       receiveData clientId chan
                               else
                                 do logger <- createLogger
                                    str <-readChunkedMessageByteString logger chan 
                                    return $ Received (fromIntegral clientId) [str]

sendData:: LibXenVChan -> [BS.ByteString] -> IO(Either (TransportError SendErrorCode)())
sendData chan mesgs = do logger <- createLogger
                         sequence $ map (\mesg -> sendChunkedMessageByteString logger chan mesg) mesgs
                         return $ Right () 

createConnection:: EndPointAddress -> Reliability -> ConnectHints-> IO(Either(TransportError ConnectErrorCode) Connection)
createConnection address _ _ = do 
           let oid = read $ BSC.unpack (endPointAddressToByteString address)
           chan <-client_init oid
	   return $ Right Connection 
                                   { Network.Transport.send = sendData chan 
                                   , Network.Transport.close = undefined
                                   }

prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to communicate with?"
                      input <- getLine
                      case reads input of
                         [(id,_)] -> return id
                         _     -> do putStrLn "Error: Please Enter a Number."
                                     loop
printEvent:: Event->IO()
printEvent e = case e of
            Received cid mesg ->  do putStrLn "Received:"
                                     let strings =  map (\m ->( BSC.unpack m)) mesg
                                     sequence_ $ map putStrLn strings
            ConnectionClosed cid -> putStrLn "closed"
            ConnectionOpened cid rel addr -> putStrLn (show addr)


main = do dom <- prompt
          transport <- mkTransport dom 
          Right endpoint  <- newEndPoint transport
          Right connection <- connect endpoint (EndPointAddress (BSC.pack (show(dom)) )) ReliableOrdered defaultConnectHints 
          forever $ do
             putStrLn "Enter text to send: "
             input <- getLine
             putStrLn ""
             Network.Transport.send connection [(BSC.pack input)]
             e <- Network.Transport.receive endpoint
             printEvent e
             putStrLn ""
