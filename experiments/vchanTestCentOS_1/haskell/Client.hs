{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Control.Concurrent

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

foreign import ccall unsafe "../include/exp1Common.h libxenvchan_wait"
    c_libxenvchan_wait:: (Ptr LibXenVChan)-> IO(CInt)

foreign import ccall unsafe "../include/exp1Common.h checkClientResponse"
    c_checkClientResponse:: (Ptr XenToolLogger)->(Ptr LibXenVChan)-> Ptr CInt ->IO (CInt)

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

createMgrCtrl logger = do mgrPath <- newCString "data/mgrVchan"
                          c_createReceiveChanP logger (fromIntegral 0:: CInt) mgrPath
ctrlWait :: Ptr LibXenVChan -> IO Int
ctrlWait ctrl = liftM fromIntegral (c_libxenvchan_wait ctrl)

dataReady :: Ptr LibXenVChan -> IO Int
dataReady ctrl = liftM fromIntegral (c_dataReady ctrl)

checkResponse :: Ptr XenToolLogger -> Ptr LibXenVChan -> IO(Int)
checkResponse logger vchan = alloca $ \ ptr -> do  (c_checkClientResponse logger vchan ptr)
                                                   response <- peek ptr
                                                   return (fromIntegral response)

main = do logger <- createLogger
          id <- getDomId
          putStrLn ("Got my Id: "++show id)         
          ctrlMgr <- createMgrCtrl logger

                        -- 1/2 second delay
          let loop = do threadDelay(10000* 50)
                       
                        -- is there something to be read?
                        size <- dataReady ctrlMgr
                        if size > 0 then
                          do response <- checkResponse logger ctrlMgr 
                             putStrLn ("Received from MGR: "++ (show response))
                             loop
                        else
                          loop
           in loop
          destroyLogger logger
          
