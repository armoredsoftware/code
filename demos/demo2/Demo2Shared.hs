module Demo2Shared where

-- utility libraries
import Data.Binary
import Data.ByteString (ByteString, pack, append, empty)
import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe (unsafePerformIO)

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
                     
-- Primitive types
type PCR = Word8
type Nonce = ByteString
type Signature = ByteString
type TPMRequest = Word8 -- Request = (Mask, Nonce)
type Quote = (([PCR], Nonce), Signature)--simulates TPM 

--Request
type Request = (DesiredEvidence, TPMRequest, Nonce)
type DesiredEvidence = [EvidenceDescriptor]
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
  show D0 = "Evidence Piece #0"
  show D1 = "Evidence Piece #1"
  show D2 = "Evidence Piece #2"
   

--Response
type Response = (EvidencePackage, QuotePackage)
type EvidencePackage = (Evidence, Nonce, Signature)
type Evidence = [EvidencePiece]
type EvidencePiece = ByteString --for now 
type Hash = ByteString
type QuotePackage = (Quote, Hash, Signature)


ePack :: Evidence -> Nonce -> ByteString
ePack e n = (B.concat e) `append` n

qPack :: Quote -> Hash -> ByteString
qPack q@((pcrsIn, nonce), sig) hash = 
  (tPack (pcrsIn, nonce)) `append` sig `append` hash
  
tPack :: ([PCR], Nonce) -> ByteString
tPack (pcrs, nonce) = pack pcrs `append` nonce

doHash :: ByteString -> ByteString
doHash = hash


getKeys :: (PrivateKey, PublicKey)
getKeys = unsafePerformIO $ readKeys

getPriKey :: PrivateKey
getPriKey = fst getKeys

getPubKey :: PublicKey
getPubKey = snd getKeys

readKeys :: IO (PrivateKey, PublicKey)
readKeys =
     do handle <- openFile "keys.txt" ReadMode
        priString <- hGetLine handle
        pubString <- hGetLine handle
	let pri :: PrivateKey
            pri = read priString
            pub :: PublicKey
            pub = read pubString
        hClose handle
	return (pri, pub)