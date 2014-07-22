{-# LANGUAGE ScopedTypeVariables #-}


import JSONCaster
import Data.Aeson
--vchan library
import VChanUtil
import System.IO

import Data.Binary
import Data.ByteString (ByteString, cons, empty)
import Data.Bits
import Control.Monad

data EvidencePiece = M0 M0Rep 
                   | M1 M1Rep
                   | M2 M2Rep deriving (Eq, Ord, Show)

type M0Rep = ByteString
type M1Rep = ByteString
type M2Rep = ByteString

instance Binary EvidencePiece where
         put (M0 req) = do put (0::Word8);
                             put req;
         put(M1 quote) =  do put (1::Word8);
                               put quote;
         put(M2 res)= do put(2::Word8);
                           put res;
                                            
         get = do t<- get :: Get Word8
                  case t of
                    0 -> do req <- get
                            return (M0 req)
                    1 -> do quote <- get
                            return (M1 quote)
                    2 -> do res <- get
                            return (M2 res)

data EvidenceDescriptor = D0 | D1 | D2 deriving(Eq, Ord) --for now

instance Binary EvidenceDescriptor where
  put D0 = do put (0::Word8)
  put D1 = do put (1::Word8)
  put D2 = do put (2::Word8)
           
  get = do t<- get :: Get Word8
           case t of
               0 -> return D0
               1 -> return D1
               2 -> return D2
                    
instance Show EvidenceDescriptor where
  show D0 = "Measurement #0"
  show D1 = "Measurement #1"
  show D2 = "Measurement #2"
  
prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID is the Attester?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop
  
  
main :: IO ()
main = do
  measurerID <- prompt
  chan <- server_init measurerID
  forever $ process chan
  return ()

  
process :: LibXenVChan -> IO ()
process chan = do
  
  ctrlWait chan
  logger <- createLogger
  ed :: EvidenceDescriptor <- stripED $ fromJust (decode (fromChunks (readChunkedMessageString logger chan)) :: Maybe EvidenceDescriptor)
--(receive chan) :: Maybe EvidenceDescriptorWrapper)
  let ep = toChunks (encode (EPW (measure ed)))
  logger <- createLogger
  sendChunkedMessageString logger chan ep 
  return ()


measure :: EvidenceDescriptor -> EvidencePiece
measure ed = case ed of 
  D0 -> M0 m0Val
  D1 -> M1 m1Val
  D2 -> M2 m2Val
                        


m0Val :: M0Rep
m0Val = cons (bit 0) empty

m1Val :: M1Rep
m1Val = cons (bit 1) empty

m2Val :: M2Rep
m2Val = cons (bit 2) empty
