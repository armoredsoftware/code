{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables #-}
module VChanUtil 
(getDomId
,client_init
,server_init
,receive
,send
,close
,printf
,createLogger
,destroyLogger
,createMgrChan_Srv
,createMgrChan_Client
,createSrvCtrl 
,createClientCtrl 
,sendString
,sendBlob
,getBlob
,ctrlWait
,ctrlClose
,dataReady
,checkMessage
,sendChunkedMessageString
,readChunkedMessageString
,sendChunkedMessageByteString
,readChunkedMessageByteString
,getBufferSpace
,XenToolLogger
,LibXenVChan
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString as BS
import Codec.Compression.GZip

data XenToolLogger
data LibXenVChan

foreign import ccall unsafe "exp1Common.h getDomId"
    c_getDomId:: IO CInt

foreign import ccall unsafe "exp1Common.h printf"
    c_printf:: CString-> IO (CInt)

foreign import ccall unsafe "exp1Common.h createDebugLogger"
    c_createDebugLogger:: IO (Ptr XenToolLogger) 

foreign import ccall unsafe "exp1Common.h xtl_logger_destroy"
    c_destroyDebugLogger:: Ptr XenToolLogger -> IO () 

foreign import ccall unsafe "exp1Common.h createReceiveChanP"
    c_createReceiveChanP:: Ptr XenToolLogger -> CInt-> CString -> IO (Ptr LibXenVChan) 

foreign import ccall unsafe "exp1Common.h createTransmitChanP"
    c_createTransmitChanP:: Ptr XenToolLogger -> CInt-> CInt-> CString -> IO (Ptr LibXenVChan) 

foreign import ccall unsafe "exp1Common.h libxenvchan_wait"
    c_libxenvchan_wait:: (Ptr LibXenVChan)-> IO(CInt)

foreign import ccall unsafe "exp1Common.h libxenvchan_close"
    c_libxenvchan_close:: (Ptr LibXenVChan)-> IO()

foreign import ccall unsafe "exp1Common.h readClientMessage"
    c_readClientMessage:: (Ptr XenToolLogger)->(Ptr LibXenVChan)-> CString -> Ptr CInt->IO (CInt)

foreign import ccall unsafe "exp1Common.h sendClientMessage"
    c_sendClientMessage:: (Ptr XenToolLogger)->(Ptr LibXenVChan)-> CString ->CInt-> IO (CInt)

foreign import ccall unsafe "exp1Common.h libxenvchan_data_ready"
    c_dataReady:: (Ptr LibXenVChan)-> IO (CInt)

foreign import ccall unsafe "exp1Common.h libxenvchan_buffer_space"
    c_bufferSpace:: (Ptr LibXenVChan)-> IO (CInt)

foreign import ccall unsafe "exp1Common.h readChunkedMessage"
    c_readChunkedMessage :: Ptr XenToolLogger -> Ptr LibXenVChan -> Ptr CInt->IO CString

foreign import ccall unsafe "exp1Common.h sendChunkedMessage"
    c_sendChunkedMessage :: Ptr XenToolLogger -> Ptr LibXenVChan -> CString->CInt->IO(CInt)

getDomId :: IO Int
getDomId = do x <- c_getDomId
              return (fromIntegral x)


printf :: String -> IO (CInt)
printf msg = do x <- (newCString msg)
                c_printf x

-- A logger should be created before any VChan Communication
createLogger :: IO (Ptr XenToolLogger)
createLogger = c_createDebugLogger

-- Destroy logger after you close the VChan
destroyLogger :: Ptr XenToolLogger -> IO()
destroyLogger logger =  c_destroyDebugLogger logger


-- Create a Vchan for Dom-0 to connect
createMgrChan_Srv :: Ptr XenToolLogger -> IO (Ptr LibXenVChan)
createMgrChan_Srv logger = do createSrvCtrlP logger 0 "data/mgrVchan"


-- Dom-0 should call this func to connect as a client to an already created VChan
createMgrChan_Client :: Ptr XenToolLogger -> Int -> IO (Ptr LibXenVChan)
createMgrChan_Client logger srvId = do createClientCtrlP logger srvId 0 "data/mgrVchan"

--Create a VChan as a server for that clientId will later connect to 
--Wrapper
createSrvCtrl :: Ptr XenToolLogger -> Int-> IO (Ptr LibXenVChan)
createSrvCtrl logger clientId = let path = "data/serverVchan"
                                 in  createSrvCtrlP logger clientId path

--Underlying function used to create a Vchan as a Server
createSrvCtrlP :: Ptr XenToolLogger -> Int-> String -> IO (Ptr LibXenVChan)
createSrvCtrlP logger clientId p = do path <- newCString p 
                                      ctrl  <-c_createReceiveChanP logger 
                                              (fromIntegral clientId:: CInt)
                                              path
                                      free path
                                      return ctrl

--Client dom connects to an existing VChan in which the client has permission 
createClientCtrl :: Ptr XenToolLogger -> Int -> IO (Ptr LibXenVChan)
createClientCtrl logger srvId = do clientId <- getDomId
                                   let path = "data/serverVchan"
                                   createClientCtrlP logger srvId clientId path

--Underlying function used to connect to a VChan as a Client
createClientCtrlP :: Ptr XenToolLogger->Int->Int->String->IO (Ptr LibXenVChan)
createClientCtrlP logger srvId clientId p = do path <- newCString p 
                                               putStrLn p
                                               chan <- c_createTransmitChanP logger 
                                                        (fromIntegral srvId :: CInt) 
                                                        (fromIntegral clientId :: CInt) 
                                                        path
                                               free path
                                               return chan
--Send a string via the input vchan
sendString :: Ptr XenToolLogger -> Ptr LibXenVChan -> String -> IO Int
sendString logger ctrl str = do  msg <- newCString str
                                 res <- liftM fromIntegral $ 
                                         c_sendClientMessage
                                          logger
                                          ctrl
                                          msg
                                          (fromIntegral $ length str)
                                 free msg
                                 return res
sendBlob logger ctrl blob = let size = fromIntegral (BS.length  blob):: CInt

                                 in BS.useAsCStringLen blob (\(message,sz) -> do
                                       liftM fromIntegral $ c_sendClientMessage
                                                             logger
                                                             ctrl
                                                             message
                                                             (fromIntegral sz))


getBlob :: Ptr XenToolLogger -> Ptr LibXenVChan -> Int->IO BS.ByteString
getBlob logger ctrl dataSize= do  allocaArray0 dataSize $ \( ptr::CString) ->
                                      alloca $ \(size :: Ptr CInt) -> do
                                              poke size (fromIntegral dataSize:: CInt)
                                              c_readClientMessage
                                                logger
                                                ctrl
                                                ptr
                                                size
                                              sz<- peek size
                                              response <- BS.packCStringLen
                                                  (ptr, fromIntegral sz:: Int)
                                              return $ response

getBufferSpace :: Ptr LibXenVChan-> IO(Int64)
getBufferSpace vchan =  liftM fromIntegral $ (c_bufferSpace vchan)

--Wait until an event happens on the input VChan
ctrlWait :: Ptr LibXenVChan -> IO Int
ctrlWait ctrl = liftM fromIntegral (c_libxenvchan_wait ctrl)

ctrlClose :: Ptr LibXenVChan -> IO ()
ctrlClose ctrl =c_libxenvchan_close ctrl

--Returns the size of the data ready to be read from the VChan
dataReady :: Ptr LibXenVChan -> IO Int
dataReady ctrl = liftM fromIntegral (c_dataReady ctrl)

--Use with dataReady to get a certain amount of data from a VChan
checkMessage :: (Binary e)=>Ptr XenToolLogger -> Ptr LibXenVChan -> Int->IO e
checkMessage logger vchan dataSize= allocaArray0 dataSize $ \( ptr::CString) ->
                              alloca $ \(size :: Ptr CInt) -> do 
                                              poke size (fromIntegral dataSize:: CInt)
                                              c_readClientMessage 
                                                logger 
                                                vchan 
                                                ptr 
                                                size
                                              sz<- peek size
                                              response <- BS.packCStringLen
                                                  (ptr, fromIntegral sz:: Int) 
                                              return $ decode $ decompress $
                                                   LazyBS.fromStrict response


sendChunkedMessageString :: Ptr XenToolLogger->Ptr LibXenVChan->String-> IO(Int)
sendChunkedMessageString logger vchan msg =  do str<- newCString msg
                                                let size = length msg
                                                res <- liftM fromIntegral $ c_sendChunkedMessage
                                                         logger
                                                         vchan
                                                         str
                                                         (fromIntegral size :: CInt)
                                                free str
                                                return res
                                           
sendChunkedMessageByteString :: Ptr XenToolLogger->Ptr LibXenVChan->BS.ByteString-> IO(Int)
sendChunkedMessageByteString logger vchan msg = BS.useAsCStringLen msg (\(message,sz) -> 
                                                        liftM fromIntegral $ c_sendChunkedMessage
                                                             logger
                                                             vchan
                                                             message
                                                             (fromIntegral sz :: CInt))
                                          

readChunkedMessageString :: Ptr XenToolLogger-> Ptr LibXenVChan -> IO String
readChunkedMessageString logger vchan = do alloca $ \(size:: Ptr CInt)-> 
                                             do ptr <-c_readChunkedMessage logger vchan size
                                                sz <- peek size
 
                                                str <-peekCStringLen (ptr,fromIntegral sz ::Int)
                                                free ptr
                                                return str
 
readChunkedMessageByteString :: Ptr XenToolLogger-> Ptr LibXenVChan -> IO BS.ByteString
readChunkedMessageByteString logger vchan = do alloca $ \(size :: Ptr CInt) ->
                                                  do ptr <-c_readChunkedMessage logger vchan size
                                                     sz <- peek size
                                                     str <-BS.packCStringLen (ptr,fromIntegral sz ::Int)
                                                     free ptr
                                                     return str
                                                         

-- #################################################################################3
client_init:: Int-> IO (Ptr LibXenVChan)
client_init otherDom = do logger <- createLogger
                          chan <-createClientCtrl logger otherDom
                          destroyLogger logger
                          return chan
                           
                        

server_init::Int-> IO (Ptr LibXenVChan)
server_init otherDom = do logger <- createLogger
                          chan <-createSrvCtrl logger otherDom
                          destroyLogger logger
                          return chan

send :: Binary e => Ptr LibXenVChan -> e -> IO(Int)
send chan packet = do let msg = LazyBS.toStrict $ compress $ encode packet 
                      logger <- createLogger
                      res <-BS.useAsCStringLen msg (\(message,sz)-> 
                             liftM fromIntegral $ c_sendChunkedMessage
                                 logger chan message (fromIntegral sz ::CInt))
                      destroyLogger logger
                      return res


receive :: Binary e => Ptr LibXenVChan-> IO e
receive chan = do logger <- createLogger
                  alloca $ \(size :: Ptr CInt) -> do
                   ptr <-c_readChunkedMessage logger chan size
                   sz <- peek size
                   str <- BS.packCStringLen (ptr, fromIntegral sz:: Int)
                   free ptr
                   destroyLogger logger
                   return $ decode $ decompress $ LazyBS.fromStrict str
close :: Ptr LibXenVChan -> IO ()
close chan = ctrlClose chan