{-# LANGUAGE ScopedTypeVariables #-}

--vchan library
import VChanUtil

-- crypto libraries
import Crypto.Random
import Crypto.PubKey.HashDescr
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Crypto.Hash.MD5(hash)

-- utility libraries
import Control.Exception hiding (evaluate)
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString, pack, append)
import qualified Data.ByteString as B
import Data.Word
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Binary
import qualified Data.Map.Lazy as M (fromList, lookup, empty)

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
type TPMRequest = Word8 --Request = (Mask, Nonce)
type Quote = (([PCR], Nonce), Signature)

--Request
type Request = (DesiredEvidence, TPMRequest, Nonce)
type DesiredEvidence = [EvidenceDescriptor]
data EvidenceDescriptor = D0 | D1 | D2  deriving (Eq,Ord)--for now

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
type Hash = ByteString
type Evidence = [EvidencePiece]
type EvidencePiece = ByteString --for now 
type QuotePackage = (Quote, Hash, Signature)


prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to Appraise?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop



main :: IO ()
main = let (mask, nonce) = mkTPMRequest [0..7] 
           req = ([D0, D1, D2], mask, nonce) in
  do chan <- sendRequest req
     response <- receiveResponse chan
     --case evaluate req response of True -> putStrLn "Appraisal Succeeded"
     return ()

                      

-- Appraisal primitives

mkTPMRequest :: [Int] -> (TPMRequest, Nonce)
mkTPMRequest mask =
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask in
      (mask', fst $ cprgGenerate 16 gen)


sendRequest :: Request -> IO (LibXenVChan)
sendRequest req = 
  do id <-getDomId
     putStrLn $ "Appraiser Domain id: "++(show id)
     other <- prompt
     chan <- client_init other
     putStrLn $ "\n" ++ "Appraiser Sending: "++(show $ Appraisal req) ++ "\n"
     send chan $ Appraisal req
     return chan

receiveResponse :: LibXenVChan -> IO Response
receiveResponse chan =  do
             ctrlWait chan
             res :: Shared <- receive chan
             case res of 
                                 Attestation response ->  do
                                                                putStrLn $ "\n" ++ "Appraiser Received: "
                                                                                               ++ (show res)++ "\n"
                                                                return response
                                 otherwise ->  throw $ ErrorCall quoteReceiveError 
                                                            -- TODO:  error handling here?

{-
evaluate :: Request -> Quote -> Bool
evaluate (mask, rnonce) (quote@(qpcrs, qnonce), signature) =
    let pcrs' = pcrSelect mask in
        if (not $ verify md5 pub (pack' quote) signature) 
           then throw $ ErrorCall "Signature could not be verified."
           else if (rnonce /= qnonce)  
                then throw $ ErrorCall "Nonce could not be verified."
                else if (pcrs' /= qpcrs) 
                     then throw $ ErrorCall "PCR not of expected value."
                     else True
-}

--type Quote = (([PCR], Nonce), Signature)
--type EvidencePackage = (Evidence, Nonce, Signature)
--type QuotePackage = (Quote, Hash, Signature)

--type Request = (DesiredEvidence, TPMRequest, Nonce)
--type Response = (EvidencePackage, QuotePackage)

evaluate :: Request -> Response -> Bool
evaluate (d, tReq, nonce) 
  ((e, eNonce, eSig), (tpmQuote@((pcrs, qNonce), qSig), hashIn, qpSig)) = 
  let pcrs' = pcrSelect tReq
      tpmBlob = tPack (pcrs, qNonce)
      eBlob = ePack e eNonce
      qBlob = qPack tpmQuote hashIn in 
  
  if (not $ verify md5 pub qBlob qpSig) 
           then throw $ ErrorCall e1
           else if (not $ verify md5 pub eBlob eSig) 
                then throw $ ErrorCall e2
                else if (not $ verify md5 pub tpmBlob qSig) 
                        then throw $ ErrorCall e3
                        else if ((hash eBlob) /= hashIn)
                                then throw $ ErrorCall e4
                                else if (nonce /= eNonce)
                                        then throw $ ErrorCall e5
                                        else if (not $ evaluateEvidence d e)
                                                then throw $ ErrorCall e6
                                                     else True
  
                                             
evaluateEvidence :: DesiredEvidence -> Evidence -> Bool
evaluateEvidence ds es = let pairs = zip ds es
                             realMap = M.fromList pairs in
                        realMap == expectedEvidence  -- More fine-grained here?

 {-where check :: EvidenceDescriptor -> Bool
       check ed = case M.lookup ed of Nothing -> False
                                                                      Just x -> x ==  -}

--expectedEvidence :: M.Map EvidenceDescriptor EvidencePiece
expectedEvidence = M.fromList [(D0, B.empty), (D1, B.empty), (D2, B.empty)]
                                             
e1 :: String
e1 = "Quote Package Signature could not be verified."
        
e2 :: String
e2 = "Evidence Package Signature could not be verified."  

e3 :: String
e3 = "TPM Signature could not be verified."  

e4 :: String
e4 = "Hash inequality suggests evidence content has been altered."  

e5 :: String
e5 = "Nonce could not be verified."

e6 :: String
e6 = "Evidence not of expected value."

doHash :: Evidence -> Nonce -> ByteString
doHash e n = let res = (B.concat e) `append` n in
  hash res

ePack :: Evidence -> Nonce -> ByteString
ePack e n = (B.concat e) `append` n      
        
qPack :: Quote -> Hash -> ByteString
qPack q@((pcrsIn, nonce), sig) hash = 
  (tPack (pcrsIn, nonce)) `append` sig `append` hash     
        
        
        
-- PCR primitives
pcrs :: [PCR]
pcrs = correct --wrong
  where correct :: [PCR]
        correct = map bit [0..7]

        wrong :: [PCR]
        --wrong = [(bit 3)] ++ (map bit [1..7])
        wrong = (map bit [0..6]) ++ [(bit 7)]

tPack :: ([PCR], Nonce) -> ByteString
tPack (pcrs, nonce) = pack pcrs `append` nonce

pcrSelect :: TPMRequest -> [PCR]
pcrSelect mask = 
    [ x | (x, n) <- zip pcrs [0..7], testBit mask n]

-- Crypto primitives
md5 :: HashDescr
md5 = hashDescrMD5

gen :: SystemRNG
gen = unsafePerformIO $ liftM cprgCreate createEntropyPool

pub :: PublicKey
pri :: PrivateKey
--(pub, pri) = fromJust $ generateWith (5,11) 255 0x10001
(pri, pub) = getKeys
--gen' :: SystemRNG
--((pub, pri), gen') = generate gen 255 3


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


--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"
