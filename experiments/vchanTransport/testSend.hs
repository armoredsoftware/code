import VChanUtil
import Control.Monad
import qualified Data.ByteString.Char8 as BSC

prompt:: IO (Int)
--prompt = return 6
prompt= loop
      where loop = do putStrLn "To which Domain ID would you like to send ?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop


main= do id <-prompt
         putStrLn $ "ID: "++(show id)
         logger <- createLogger
         sChan <- server_init id
         ctrlWait sChan
         
         chan <- client_init id
         forever $ do putStrLn "Input String:"
                      input <- getLine
                      sendChunkedMessageByteString logger chan $ BSC.pack input
