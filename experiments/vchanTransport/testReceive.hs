{-# LANGUAGE ScopedTypeVariables #-}
import VChanUtil
import Control.Monad
import qualified Data.ByteString.Char8 as BSC

prompt:: IO (Int)
--prompt = return 6
prompt= loop
      where loop = do putStrLn "From which Domain ID would you like to receive?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop


main= do id <- prompt
         putStrLn $ "ID: "++(show id)
         chan <- server_init id
         _ <- ctrlWait chan
         sChan <- client_init id
         logger<- createLogger
         putStrLn "Waiting to receive"
         forever $ do (val::BSC.ByteString) <- readChunkedMessageByteString logger chan 
                      putStrLn $ "Received: "++(BSC.unpack val)
                      sendChunkedMessageByteString logger sChan $ BSC.pack $ "From testReceive: "++(BSC.unpack val)

