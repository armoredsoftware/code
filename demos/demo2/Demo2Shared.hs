{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Demo2Shared where

-- utility libraries
import Data.Binary
import qualified Data.ByteString as B (ByteString, pack, append, empty) 


--import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics as G

-- crypto libraries
import Crypto.PubKey.RSA
import Crypto.Hash.MD5(hash)

data Shared = Appraisal Request
              | Attestation Response
              | Result Bool



instance Show Shared where
    show (Appraisal app) = "Appraisal: " ++ (show app)
    show (Attestation att) = "Attestation: " ++ (show att)
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."
    
instance Binary Shared where
  put (Appraisal req)              = do put (0::Word8)
                                        put req
  put(Attestation quote)           = do put (1::Word8)
  		  		     	put quote
  put(Result res)                  = do put(2::Word8)
                                        put res

  get = do t<- get :: Get Word8
           case t of
             0 -> do req <- get
                     return (Appraisal req)
             1 -> do quote <- get
                     return (Attestation quote)
             2 -> do res <- get
                     return (Result res)
                     
instance Binary EvidencePiece where
         put (Demo2Shared.M0 req) = do put (0::Word8);
                                       put req;
         put(Demo2Shared.M1 quote) =  do put (1::Word8);
                                         put quote;
         put(Demo2Shared.M2 res)= do put(2::Word8);
                                     put res;
                                            
         get = do t<- get :: Get Word8
                  case t of
                    0 -> do req <- get
                            return (Demo2Shared.M0 req)
                    1 -> do quote <- get
                            return (Demo2Shared.M1 quote)
                    2 -> do res <- get
                            return (Demo2Shared.M2 res)



-- Primitive types
type PCR = Word8
type Nonce = B.ByteString
type Signature = B.ByteString
type TPMRequest = Word8 -- Request = (Mask, Nonce)
type Quote = (([PCR], Nonce), Signature)--simulates TPM 

--Request
type Request = (DesiredEvidence, TPMRequest, Nonce)
type DesiredEvidence = [EvidenceDescriptor]
data EvidenceDescriptor = D0 | D1 | D2 deriving(Eq, Ord, Generic) --for now

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
   

--Response
type Response = (EvidencePackage, QuotePackage)
type EvidencePackage = (Evidence, Nonce, Signature)
type Evidence = [EvidencePiece]
 
data EvidencePiece = M0 M0Rep 
                   | M1 M1Rep
                   | M2 M2Rep deriving (Eq, Ord, Show, Generic)
                         
type Hash = B.ByteString
type QuotePackage = (Quote, Hash, Signature)


type M0Rep = B.ByteString
type M1Rep = B.ByteString
type M2Rep = B.ByteString


ePack :: Evidence -> Nonce -> B.ByteString
ePack e n = (ePack' e) `B.append` n

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> B.ByteString
ePack' es = foldr f B.empty es
  where f (Demo2Shared.M0 x) y = x `B.append` y -- (i.e. (toByteString x) `append` y )
        f (Demo2Shared.M1 x) y = x `B.append` y
        f (Demo2Shared.M2 x) y = x `B.append` y

qPack :: Quote -> Hash -> B.ByteString
qPack q@((pcrsIn, nonce), sig) hash = 
  (tPack (pcrsIn, nonce)) `B.append` sig `B.append` hash
  
tPack :: ([PCR], Nonce) -> B.ByteString
tPack (pcrs, nonce) = B.pack pcrs `B.append` nonce


doHash :: B.ByteString -> B.ByteString
doHash = hash



