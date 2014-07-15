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
import Data.ByteString (ByteString, pack, append, empty)
import qualified Data.ByteString as B
import Data.Word
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Binary

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
data EvidenceDescriptor = D0 | D1 | D2  --for now

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

prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID is the Appraiser?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop
                                    
                                    
measurePrompt :: IO (Int)
measurePrompt = loop
      where loop = do putStrLn "Which Domain ID is the Measurer?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop


main = do
  appraiserID <- prompt
  chan <- server_init appraiserID
  req <- receiveRequest chan
  resp <- mkResponse req
  sendResponse chan resp
  return ()


mkResponse :: Request -> IO Response
mkResponse (desiredE, desiredPCRs, nonce) = do
  measurerID <- measurePrompt
  chan <- client_init measurerID
  eList <- mapM (getEvidencePiece chan) desiredE
  let evPack = signEvidence eList nonce
      quote = mkSignedTPMQuote desiredPCRs nonce
      hash = doHash eList nonce
      quoPack = signQuote quote hash
        
  return (evPack, quoPack) {-where
  ep = ([empty], empty, empty)
  qp = ((([bit 0], empty), empty), empty, empty) -}


doHash :: Evidence -> Nonce -> ByteString
doHash e n = let res = (B.concat e) `append` n in
  hash res

signQuote :: Quote -> Hash -> QuotePackage
signQuote q@((pcrsIn, nonce), sig) hash = case sign Nothing md5 pri res of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (q, hash, signature) 
 where res =  qPack q hash
--type Quote = (([PCR], Nonce), Signature)--simulates TPM   
  
qPack :: Quote -> Hash -> ByteString
qPack q@((pcrsIn, nonce), sig) hash = 
  (pack' (pcrsIn, nonce)) `append` sig `append` hash
  

signEvidence :: Evidence -> Nonce -> EvidencePackage
signEvidence e n = case sign Nothing md5 pri res of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (e, n, signature) 
         
   where res = ePack e n

ePack :: Evidence -> Nonce -> ByteString
ePack e n = (B.concat e) `append` n

getEvidencePiece :: LibXenVChan -> EvidenceDescriptor -> IO EvidencePiece
getEvidencePiece chan ed = do
  putStrLn $ "\n" ++ "Attestation Agent Sending: " ++ (show ed)
  send chan $ ed
  ctrlWait chan
  evidence :: EvidencePiece <- receive chan --TODO:  error handling
  putStrLn $ "Received: " ++ (show evidence)
  return evidence
  

receiveRequest :: LibXenVChan -> IO Request
receiveRequest chan = do
  ctrlWait chan
  res :: Shared <- receive chan
  case res of
    Appraisal req -> do
      putStrLn $ "\n" ++ "Attester Received: " ++ (show res) ++ "\n"
      return req
    otherwise -> throw $ ErrorCall requestReceiveError
      
sendResponse :: LibXenVChan -> Response-> IO ()   
sendResponse chan resp = do
  putStrLn $ "Attester Sending: " ++ (show $ Attestation resp) ++ "\n"
  send chan $ Attestation resp
  return () 

mkSignedTPMQuote :: TPMRequest -> Nonce -> Quote
mkSignedTPMQuote mask nonce =
    let pcrs' = pcrSelect mask
        quote = (pcrs', nonce) in
      case sign Nothing md5 pri $ pack' quote of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (quote, signature) 
                 
  -- PCR primitives
pcrs :: [PCR]
pcrs = correct --wrong
  where correct :: [PCR]
        correct = map bit [0..7]

        wrong :: [PCR]
        --wrong = [(bit 3)] ++ (map bit [1..7])
        wrong = (map bit [0..6]) ++ [(bit 7)]

pack' :: ([PCR], Nonce) -> ByteString
pack' (pcrs, nonce) = pack pcrs `append` nonce

pcrSelect :: TPMRequest -> [PCR]
pcrSelect mask = 
    [ x | (x, n) <- zip pcrs [0..7], testBit mask n] 

-- Crypto primitives
md5 :: HashDescr
md5 = hashDescrMD5

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
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"
  