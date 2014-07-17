{-# LANGUAGE ScopedTypeVariables #-}

--our libraries
import Demo2Shared

--vchan library
import VChanUtil

-- crypto libraries
import Crypto.Random
import Crypto.PubKey.HashDescr
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
--import Crypto.Hash.MD5(hash)

-- utility libraries
import Control.Exception hiding (evaluate)
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString, pack, append, empty)
import qualified Data.ByteString as B
--import Data.Word
import System.IO
import System.IO.Unsafe (unsafePerformIO)
--import Data.Binary

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

main :: IO ()
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
  --close chan
  let evPack = signEvidence eList nonce
      quote = mkSignedTPMQuote desiredPCRs nonce
      hash = doHash $ ePack eList nonce
      quoPack = signQuote quote hash
        
  return (evPack, quoPack) {-where
  ep = ([empty], empty, empty)
  qp = ((([bit 0], empty), empty), empty, empty) -}


signQuote :: Quote -> Hash -> QuotePackage
signQuote quote hash =
  case sign Nothing md5 pri res of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (quote, hash, signature) 
 where res =  qPack quote hash
--type Quote = (([PCR], Nonce), Signature)--simulates TPM   
  

signEvidence :: Evidence -> Nonce -> EvidencePackage
signEvidence e n =
  case sign Nothing md5 pri res of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (e, n, signature) 
         
   where res = ePack e n


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
      case sign Nothing md5 pri $ tPack quote of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (quote, signature) 
                 
  -- PCR primitives
pcrsLocal :: [PCR]
pcrsLocal = a --b
  where a :: [PCR]
        a = map bit [0..7]

        b :: [PCR]
        b = [(bit 3)] ++ (map bit [1..7])
     

pcrSelect :: TPMRequest -> [PCR]
pcrSelect mask = 
    [ x | (x, n) <- zip pcrsLocal [0..7], testBit mask n] 

-- Crypto primitives
md5 :: HashDescr
md5 = hashDescrMD5

pub :: PublicKey
pri :: PrivateKey
--(pub, pri) = fromJust $ generateWith (5,11) 255 0x10001
(pri, pub) = getKeys
--gen' :: SystemRNG
--((pub, pri), gen') = generate gen 255 3


--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"
  





getKeys :: (PrivateKey, PublicKey)
getKeys = unsafePerformIO $ readKeys

getPriKey :: PrivateKey
getPriKey = fst getKeys

getPubKey :: PublicKey
getPubKey = snd getKeys

readKeys :: IO (PrivateKey, PublicKey)
readKeys = do
  handle <- openFile attKeyFileName ReadMode
  priString <- hGetLine handle
  pubString <- hGetLine handle
  let pri :: PrivateKey
      pri = read priString
      pub :: PublicKey
      pub = read pubString
  hClose handle
  return (pri, pub)
  
  
attKeyFileName :: String
attKeyFileName = "attKeys.txt"