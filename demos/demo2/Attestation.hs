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

-- utility libraries
import Control.Exception hiding (evaluate)
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString, pack, append, empty)
import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe (unsafePerformIO)

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

-- Attestation primitives  
  
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
        
  return (evPack, quoPack)

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

signQuote :: Quote -> Hash -> QuotePackage
signQuote quote hash =
  case sign Nothing md5 pri res of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (quote, hash, signature) 
 where res =  qPack quote hash

signEvidence :: Evidence -> Nonce -> EvidencePackage
signEvidence e n =
  case sign Nothing md5 pri res of
         Left err -> throw . ErrorCall $ show err
         Right signature -> (e, n, signature) 
         
   where res = ePack e n

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
(pri, pub) = getKeys

-- Utility functions to get keys
attKeyFileName :: String
attKeyFileName = "attKeys.txt"

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
  

--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"