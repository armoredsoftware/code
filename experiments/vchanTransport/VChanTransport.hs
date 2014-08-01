
import Network.Transport
import VChanUtil
import qualified Data.ByteString as BS
mkTransport server id
	| server = return Transport
                          { newEndPoint = serverEnd id
                          , closeTransport = closeT
                          }
        | not server = return Transport
                          { newEndPoint = clientInit id
                          , closeTransport = closeT 
                          }

clientInit :: Int -> IO(Either (TransportError NewEndPointErrorCode) EndPoint)
clientInit oid = do vchan <- client_init oid
                    myId <- getDomId
                    return $ Right EndPoint{ Network.Transport.receive = undefined 
                                           , address = EndPointAddress (BS.pack [fromIntegral myId])
                                           , connect = undefined
                                           , newMulticastGroup = return $ Left (TransportError NewMulticastGroupUnsupported "Unsupported Method")
                                           , resolveMulticastGroup = \_-> return $ Left (TransportError ResolveMulticastGroupUnsupported "Unsupported Method")
                                           , closeEndPoint = undefined
                                           }

serverEnd id = undefined

closeT  = undefined


main = do transport <- mkTransport True 5 
          Right endpoint  <- newEndPoint transport
          Right connection <- connect endpoint (EndPointAddress (BS.pack [1])) ReliableOrdered defaultConnectHints 
          Network.Transport.send connection [(BS.pack [1..10])]
          e <- Network.Transport.receive endpoint
          case e of
            Received cid mesg -> putStrLn "test"
            ConnectionClosed cid -> putStrLn "test"
            ConnectionOpened cid rel addr -> putStrLn (show addr) 
            
          putStrLn ""
