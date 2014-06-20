module Demo2V2 where

import Data.Word
import System.Random 
import Data.Binary 
import Data.ByteString     
import qualified Data.ByteString.Lazy.Internal as Lazy
import Codec.Crypto.RSA
main :: IO ()
main = do {
          nonce <- rollHugeDice;
          print nonce;
          
          let request = (constructARequest nonce) in
              --appraiserMan = Appraiser { keyPair = generateKeyPair } in
                --print request;
                print $ serializeRequest request;
          print "end";
          
          }
        

--data Appraiser = Appraiser{
 --               keyPair :: (PublicKey, PrivateKey, RandomGen StdGen)
  --              
  --              }


data Request = Request{
        requestnonce :: Nonce,
        pcrList :: Word16,
        instructions :: String
        } deriving (Show) 
instance Binary Request where
        put x = do
                        put (requestnonce x)
                        put (pcrList x)
                        put (instructions x)
        get = do
                        nonce <- get
                        plist <- get
                        insts <- get
                        return Request{ requestnonce = nonce, pcrList = plist, instructions = insts }
                        
type Nonce = Int
type PCRList= Word16
type RestofRequest=String

data ResponseQuote = ResponseQuote {
        pcrc :: Word16,
        hash :: Word16
        }
                                    

data ResponseM = ResponseM { 
        message :: String,
        responsenonce :: Nonce
        }
        
data AppState = BuildRequest | SendConfirmRequest | AwaitConfirmRespond | ReceiveConfirm | ConfirmProtocol
        | SendRequest | AwaitRequestRespond | ReceiveRequestRespond | CheckRequestResponse
        | MakeTrustDecision deriving Enum

serializeRequest :: Request -> Lazy.ByteString
serializeRequest r = encode r

--encrypteRequest :: Request 

  
constructARequest :: Nonce -> Request
constructARequest n = Request {
                                requestnonce = n,
                                pcrList = (22 :: Word16),
                                instructions = "Do this, this, and this, then this, then that.."
                                }
                                
                                
                                

            
rollHugeDice :: IO Int
rollHugeDice = getWrappedRandomIntInRange 
          (minBound :: Int) (maxBound :: Int)

getWrappedRandomIntInRange :: Int -> Int -> IO Int
getWrappedRandomIntInRange l h =  
        getStdRandom (randomR (l, h)) 
        
