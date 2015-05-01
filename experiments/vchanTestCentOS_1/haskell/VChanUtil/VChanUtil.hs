{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables #-}
module VChanUtil 
(getDomId
,client_init
,maybe_client_init
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

type XenToolLogger = Ptr Logger_internal
data Logger_internal
type  LibXenVChan = Ptr VChan_internal
data VChan_internal

foreign import ccall safe "exp1Common.h getDomId"
    c_getDomId:: IO CInt

foreign import ccall safe "exp1Common.h isNull"
    c_isNull:: (LibXenVChan) -> IO CInt

foreign import ccall safe "exp1Common.h printf"
    c_printf:: CString-> IO (CInt)

foreign import ccall safe "exp1Common.h createDebugLogger"
    c_createDebugLogger:: IO (XenToolLogger) 

foreign import ccall safe "exp1Common.h xtl_logger_destroy"
    c_destroyDebugLogger:: XenToolLogger -> IO () 

foreign import ccall safe "exp1Common.h createReceiveChanP"
    c_createReceiveChanP:: XenToolLogger -> CInt-> CString -> IO (LibXenVChan) 

foreign import ccall safe "exp1Common.h vchan_client_init"
    c_client_init:: XenToolLogger -> CInt-> IO (LibXenVChan) 

foreign import ccall safe "exp1Common.h vchan_maybe_client_init"
    c_maybe_client_init:: XenToolLogger -> CInt-> IO (LibXenVChan) 

foreign import ccall safe "exp1Common.h vchan_server_init"
    c_server_init:: XenToolLogger -> CInt-> IO (LibXenVChan) 

foreign import ccall safe "exp1Common.h createTransmitChanP"
    c_createTransmitChanP:: XenToolLogger -> CInt-> CInt-> CString -> IO (LibXenVChan) 

foreign import ccall safe "exp1Common.h libxenvchan_wait"
    c_libxenvchan_wait:: (LibXenVChan)-> IO(CInt)

foreign import ccall safe "exp1Common.h libxenvchan_close"
    c_libxenvchan_close:: (LibXenVChan)-> IO()

foreign import ccall safe "exp1Common.h readClientMessage"
    c_readClientMessage:: (XenToolLogger)-> LibXenVChan -> CString -> Ptr CInt->IO (CInt)

foreign import ccall safe "exp1Common.h sendClientMessage"
    c_sendClientMessage:: (XenToolLogger)-> LibXenVChan -> CString ->CInt-> IO (CInt)

foreign import ccall safe "exp1Common.h libxenvchan_data_ready"
    c_dataReady:: LibXenVChan -> IO (CInt)

foreign import ccall safe "exp1Common.h libxenvchan_buffer_space"
    c_bufferSpace:: LibXenVChan -> IO (CInt)

foreign import ccall safe "exp1Common.h readChunkedMessage"
    c_readChunkedMessage :: XenToolLogger -> LibXenVChan -> Ptr CInt->IO CString

foreign import ccall safe "exp1Common.h sendChunkedMessage"
    c_sendChunkedMessage :: XenToolLogger -> LibXenVChan -> CString->CInt->IO(CInt)

getDomId :: IO Int
getDomId = do x <- c_getDomId
              return (fromIntegral x)


printf :: String -> IO (CInt)
printf msg = do x <- (newCString msg)
                c_printf x

-- A logger should be created before any VChan Communication
createLogger :: IO (XenToolLogger)
createLogger = c_createDebugLogger

-- Destroy logger after you close the VChan
destroyLogger :: XenToolLogger -> IO()
destroyLogger logger =  c_destroyDebugLogger logger


-- Create a Vchan for Dom-0 to connect
createMgrChan_Srv :: XenToolLogger -> IO (LibXenVChan)
createMgrChan_Srv logger = do createSrvCtrlP logger 0 "data/mgrVchan"


-- Dom-0 should call this func to connect as a client to an already created VChan
createMgrChan_Client :: XenToolLogger -> Int -> IO (LibXenVChan)
createMgrChan_Client logger srvId = do createClientCtrlP logger srvId 0 "data/mgrVchan"

--Deprecated
--Create a VChan as a server for that clientId will later connect to 
--Wrapper
createSrvCtrl :: XenToolLogger -> Int-> IO (LibXenVChan)
createSrvCtrl logger clientId = let path = "data/serverVchan"
                                 in  createSrvCtrlP logger clientId path
--Deprecated
--Underlying function used to create a Vchan as a Server
createSrvCtrlP :: XenToolLogger -> Int-> String -> IO (LibXenVChan)
createSrvCtrlP logger clientId p = do path <- newCString p 
                                      ctrl  <-c_createReceiveChanP logger 
                                              (fromIntegral clientId:: CInt)
                                              path
                                      free path
                                      return ctrl
--Deprecated
--Client dom connects to an existing VChan in which the client has permission 
createClientCtrl :: XenToolLogger -> Int -> IO (LibXenVChan)
createClientCtrl logger srvId = do clientId <- getDomId
                                   let path = "data/serverVchan"
                                   createClientCtrlP logger srvId clientId path

--Deprecated
--Underlying function used to connect to a VChan as a Client
createClientCtrlP :: XenToolLogger->Int->Int->String->IO (LibXenVChan)
createClientCtrlP logger srvId clientId p = do path <- newCString p 
                                               putStrLn p
                                               chan <- c_createTransmitChanP logger 
                                                        (fromIntegral srvId :: CInt) 
                                                        (fromIntegral clientId :: CInt) 
                                                        path
                                               free path
                                               return chan
--Send a string via the input vchan
sendString :: XenToolLogger -> LibXenVChan -> String -> IO Int
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


getBlob :: XenToolLogger -> LibXenVChan -> Int->IO BS.ByteString
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

getBufferSpace :: LibXenVChan-> IO(Int64)
getBufferSpace vchan =  liftM fromIntegral $ (c_bufferSpace vchan)

--Wait until an event happens on the input VChan
ctrlWait :: LibXenVChan -> IO Int
ctrlWait ctrl = liftM fromIntegral (c_libxenvchan_wait ctrl)

ctrlClose :: LibXenVChan -> IO ()
ctrlClose ctrl =c_libxenvchan_close ctrl


--Returns the size of the data ready to be read from the VChan
dataReady :: LibXenVChan -> IO Int
dataReady ctrl = liftM fromIntegral (c_dataReady ctrl)

--Use with dataReady to get a certain amount of data from a VChan
checkMessage :: (Binary e)=>XenToolLogger -> LibXenVChan -> Int->IO e
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


sendChunkedMessageString :: XenToolLogger-> LibXenVChan->String-> IO(Int)
sendChunkedMessageString logger vchan msg =  do str<- newCString msg
                                                let size = length msg
                                                res <- liftM fromIntegral $ c_sendChunkedMessage
                                                         logger
                                                         vchan
                                                         str
                                                         (fromIntegral size :: CInt)
                                                free str
                                                return res
                                           
sendChunkedMessageByteString :: XenToolLogger-> LibXenVChan->BS.ByteString-> IO(Int)
sendChunkedMessageByteString logger vchan msg = BS.useAsCStringLen msg (\(message,sz) -> 
                                                        liftM fromIntegral $ c_sendChunkedMessage
                                                             logger
                                                             vchan
                                                             message
                                                             (fromIntegral sz :: CInt))
                                          

readChunkedMessageString :: XenToolLogger-> LibXenVChan -> IO String
readChunkedMessageString logger vchan = do alloca $ \(size:: Ptr CInt)-> 
                                             do ptr <-c_readChunkedMessage logger vchan size
                                                sz <- peek size
 
                                                str <-peekCStringLen (ptr,fromIntegral sz ::Int)
                                                free ptr
                                                return str
 
readChunkedMessageByteString :: XenToolLogger-> LibXenVChan -> IO BS.ByteString
readChunkedMessageByteString logger vchan = do alloca $ \(size :: Ptr CInt) ->
                                                  do ptr <-c_readChunkedMessage logger vchan size
                                                     sz <- peek size
                                                     str <-BS.packCStringLen (ptr,fromIntegral sz ::Int)
                                                     free ptr
                                                     return str
                                                         

-- #################################################################################3
client_init:: Int-> IO (LibXenVChan)
client_init otherDom = do logger <- createLogger
                          chan <-c_client_init logger (fromIntegral otherDom :: CInt)
 --                         destroyLogger logger
                          return chan
                           
maybe_client_init:: Int-> IO ( Maybe LibXenVChan)
maybe_client_init otherDom = do logger <- createLogger
                                chan <-c_maybe_client_init logger (fromIntegral otherDom :: CInt)
 --                               destroyLogger logger
                                bool <- c_isNull chan
                                if (fromIntegral bool :: Int ) == 0 then
                                     return (Just chan)
                                else
                                     return Nothing

server_init::Int-> IO (LibXenVChan)
server_init otherDom = do logger <- createLogger
                          chan <-c_server_init logger (fromIntegral otherDom :: CInt)
                         -- destroyLogger logger
                          return chan

send :: Binary e => LibXenVChan -> e -> IO(Int)
send chan packet = do let msg = LazyBS.toStrict $ compress $ encode packet 
                      logger <- createLogger
                      res <-BS.useAsCStringLen msg (\(message,sz)-> 
                             liftM fromIntegral $ c_sendChunkedMessage
                                 logger chan message (fromIntegral sz ::CInt))
                      --destroyLogger logger
                      return res


receive :: Binary e => LibXenVChan-> IO e
receive chan = do logger <- createLogger
                  alloca $ \(size :: Ptr CInt) -> do
                   ptr <-c_readChunkedMessage logger chan size
                   sz <- peek size
                   str <- BS.packCStringLen (ptr, fromIntegral sz:: Int)
                   free ptr
--                   destroyLogger logger
                   return $ decode $ decompress $ LazyBS.fromStrict str
close :: LibXenVChan -> IO ()
close chan = ctrlClose chan
