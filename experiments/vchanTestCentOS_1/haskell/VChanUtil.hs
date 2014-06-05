{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables #-}
module VChanUtil 
(getDomId
,printf
,createLogger
,destroyLogger
,createMgrChan_Srv
,createMgrChan_Client
,createSrvCtrl 
,createClientCtrl 
,sendPacket
,sendPacketString
,sendString
,ctrlWait
,dataReady
,checkMessage
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString as BS
import Packet
import Codec.Compression.GZip

data XenToolLogger
data LibXenVChan

foreign import ccall unsafe "../include/exp1Common.h getDomId"
    c_getDomId:: IO CInt

foreign import ccall unsafe "../include/exp1Common.h printf"
    c_printf:: CString-> IO (CInt)

foreign import ccall unsafe "../include/exp1Common.h createDebugLogger"
    c_createDebugLogger:: IO (Ptr XenToolLogger) 

foreign import ccall unsafe "../include/exp1Common.h xtl_logger_destroy"
    c_destroyDebugLogger:: Ptr XenToolLogger -> IO () 

foreign import ccall unsafe "../include/exp1Common.h createReceiveChanP"
    c_createReceiveChanP:: Ptr XenToolLogger -> CInt-> CString -> IO (Ptr LibXenVChan) 

foreign import ccall unsafe "../include/exp1Common.h createTransmitChanP"
    c_createTransmitChanP:: Ptr XenToolLogger -> CInt-> CInt ->CString -> IO (Ptr LibXenVChan) 

foreign import ccall unsafe "../include/exp1Common.h libxenvchan_wait"
    c_libxenvchan_wait:: (Ptr LibXenVChan)-> IO(CInt)

foreign import ccall unsafe "../include/exp1Common.h readClientMessage"
    c_readClientMessage:: (Ptr XenToolLogger)->(Ptr LibXenVChan)-> CString -> Ptr CInt->IO (CInt)

foreign import ccall unsafe "../include/exp1Common.h sendClientMessage"
    c_sendClientMessage:: (Ptr XenToolLogger)->(Ptr LibXenVChan)-> CString ->CInt-> IO (CInt)

foreign import ccall unsafe "../include/exp1Common.h libxenvchan_data_ready"
    c_dataReady:: (Ptr LibXenVChan)-> IO (CInt)


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
createSrvCtrl logger clientId = let path = "data/serverVchan_" ++
                                           (show clientId)
                                 in  createSrvCtrlP logger clientId path

--Underlying function used to create a Vchan as a Server
createSrvCtrlP :: Ptr XenToolLogger -> Int-> String -> IO (Ptr LibXenVChan)
createSrvCtrlP logger clientId p = do path <- newCString p 
                                      c_createReceiveChanP logger 
                                       (fromIntegral clientId:: CInt)
                                       path

--Client dom connects to an existing VChan in which the client has permission 
createClientCtrl :: Ptr XenToolLogger -> Int -> Int -> IO (Ptr LibXenVChan)
createClientCtrl logger srvId clientId = let path = "data/serverVchan_"++
                                                    (show clientId)
                                          in createClientCtrlP logger srvId clientId path

--Underlying function used to connect to a VChan as a Client
createClientCtrlP :: Ptr XenToolLogger->Int->Int->String->IO (Ptr LibXenVChan)
createClientCtrlP logger srvId clientId p = do path <- newCString p 
                                               putStrLn p
                                               c_createTransmitChanP logger 
                                                (fromIntegral srvId :: CInt) 
                                                (fromIntegral clientId :: CInt)
                                                path

--Used for Debugging, sending the string version of the Packet
sendPacketString:: Ptr XenToolLogger->Ptr LibXenVChan->Packet-> IO Int 
sendPacketString logger ctrl packet = do  let packetStr = show packet
                                          msg <- newCString packetStr
                                          liftM fromIntegral $ 
                                            c_sendClientMessage logger
                                              ctrl
                                              msg
                                              (fromIntegral $ length packetStr)

--encode compress and sends the input packet to the input VChan
sendPacket:: Ptr XenToolLogger->Ptr LibXenVChan-> Packet-> IO Int
sendPacket logger ctrl packet = let msg = LazyBS.toStrict (compress (encode packet))
                                    size = fromIntegral (BS.length  msg):: CInt

                                 in BS.useAsCStringLen msg (\(message,sz) -> do 
                                     liftM fromIntegral $ c_sendClientMessage 
                                                           logger
                                                           ctrl
                                                           message
                                                           (fromIntegral sz))
--Send a string via the input vchan
sendString :: Ptr XenToolLogger -> Ptr LibXenVChan -> String -> IO Int
sendString logger ctrl str = do  msg <- newCString str
                                 liftM fromIntegral $ 
                                   c_sendClientMessage
                                     logger
                                     ctrl
                                     msg
                                     (fromIntegral $ length str)

--Wait until an event happens on the input VChan
ctrlWait :: Ptr LibXenVChan -> IO Int
ctrlWait ctrl = liftM fromIntegral (c_libxenvchan_wait ctrl)

--Returns the size of the data ready to be read from the VChan
dataReady :: Ptr LibXenVChan -> IO Int
dataReady ctrl = liftM fromIntegral (c_dataReady ctrl)

--Use with dataReady to get a certain amount of data from a VChan
checkMessage :: Ptr XenToolLogger -> Ptr LibXenVChan -> Int->IO Packet
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
                                              return $ decode $
                                                   decompress $
                                                   LazyBS.fromStrict response
