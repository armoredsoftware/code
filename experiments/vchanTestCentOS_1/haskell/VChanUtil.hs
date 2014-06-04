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
,checkResponse
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

foreign import ccall unsafe "../include/exp1Common.h checkClientResponse"
    c_checkClientResponse:: (Ptr XenToolLogger)->(Ptr LibXenVChan)-> Ptr CInt ->IO (CInt)

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
createLogger :: IO (Ptr XenToolLogger)
createLogger = c_createDebugLogger

destroyLogger :: Ptr XenToolLogger -> IO()
destroyLogger logger =  c_destroyDebugLogger logger

createMgrChan_Srv :: Ptr XenToolLogger -> IO (Ptr LibXenVChan)
createMgrChan_Srv logger = do createSrvCtrlP logger 0 "data/mgrVchan"

createMgrChan_Client :: Ptr XenToolLogger -> Int -> IO (Ptr LibXenVChan)
createMgrChan_Client logger srvId = do createClientCtrlP logger srvId 0 "data/mgrVchan"

createSrvCtrl :: Ptr XenToolLogger -> Int-> IO (Ptr LibXenVChan)
createSrvCtrl logger clientId = let path = "data/serverVchan_" ++ (show clientId)
                                 in  createSrvCtrlP logger clientId path

createSrvCtrlP :: Ptr XenToolLogger -> Int-> String -> IO (Ptr LibXenVChan)
createSrvCtrlP logger clientId p = do path <- newCString p 
                                      c_createReceiveChanP logger 
                                       (fromIntegral clientId:: CInt) path

createClientCtrl :: Ptr XenToolLogger -> Int -> Int -> IO (Ptr LibXenVChan)
createClientCtrl logger srvId clientId = let path = "data/serverVchan_"++
                                                    (show clientId)
                                          in createClientCtrlP logger srvId clientId path

createClientCtrlP :: Ptr XenToolLogger->Int->Int->String->IO (Ptr LibXenVChan)
createClientCtrlP logger srvId clientId p = do path <- newCString p 
                                               putStrLn p
                                               c_createTransmitChanP logger 
                                                (fromIntegral srvId :: CInt) 
                                                (fromIntegral clientId :: CInt)
                                                path


--decode (L.pack (read $ show $ L.unpack (encode x) ::[Word8])) :: Packet



sendPacketString logger ctrl packet = do  msg <- newCString (show packet)
                                          c_sendClientMessage logger ctrl msg (fromIntegral (length (show packet)) :: CInt)
--sendPacket:: (Ptr XenToolLogger) -> (Ptr LibXenVChan)-> Packet-> IO(int)
sendPacket logger ctrl packet = let msg = LazyBS.toStrict (compress (encode packet))
                                    size = fromIntegral (BS.length  msg) :: CInt
                                 in BS.useAsCStringLen msg (\(message,sz) -> c_sendClientMessage logger ctrl message (fromIntegral sz::CInt))

sendString logger ctrl str = do  msg <- newCString str
                                 c_sendClientMessage logger ctrl msg (fromIntegral (length str) ::CInt)

ctrlWait :: Ptr LibXenVChan -> IO Int
ctrlWait ctrl = liftM fromIntegral (c_libxenvchan_wait ctrl)

dataReady :: Ptr LibXenVChan -> IO Int
dataReady ctrl = liftM fromIntegral (c_dataReady ctrl)

checkResponse :: Ptr XenToolLogger -> Ptr LibXenVChan -> IO(Int)
checkResponse logger vchan = alloca $ \ ptr -> do  (c_checkClientResponse logger vchan ptr)
                                                   response <- peek ptr
                                                   return (fromIntegral response)

--checkMessage :: Ptr XenToolLogger -> Ptr LibXenVChan -> IO(Packet)
checkMessage logger vchan dataSize= allocaArray0 dataSize $ \( ptr::CString) ->
                              alloca $ \(size :: Ptr CInt) -> do 
                                                   poke size (fromIntegral dataSize:: CInt)
                                                   (c_readClientMessage logger vchan ptr size)
                                                   sz<- peek size
                                                   print (fromIntegral sz::Int)
                                                   response <- (BS.packCStringLen  (ptr, fromIntegral sz:: Int)) 
                                                   return(decode ((decompress (LazyBS.fromStrict response))) :: Packet)

backslashes [] accum = accum
backslashes (x:xs) [] = case x of 
                          '\\' -> backslashes xs ("////")
                          otherwise -> backslashes xs [x]
backslashes (x:xs) accum = case x of 
                             '\\' -> backslashes xs (accum++"////")
                             otherwise ->  backslashes xs (accum ++[x])



