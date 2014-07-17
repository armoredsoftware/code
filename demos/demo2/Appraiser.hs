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
--import System.IO
import System.IO.Unsafe (unsafePerformIO)
--import Data.Binary
import qualified Data.Map.Lazy as M (fromList, lookup, empty)


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
     let result = evaluate req response in 
       showDemo2EvalResult result

                      

-- Appraisal primitives

mkTPMRequest :: [Int] -> (TPMRequest, Nonce)
mkTPMRequest mask =
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask in
      (mask', fst $ cprgGenerate 16 gen)


sendRequest :: Request -> IO (LibXenVChan)
sendRequest req = do
  id <-getDomId
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
      putStrLn $ "\n" ++ "Appraiser Received: " ++ (show res)++ "\n"
      return response
    otherwise ->  throw $ ErrorCall quoteReceiveError --TODO: error handling?

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


showDemo2EvalResult :: Demo2EvalResult -> IO ()
showDemo2EvalResult (r1, r2, r3, r4, r5, ms) = do
     putStrLn $ e1 ++ show r1
     putStrLn $ e2 ++ show r2
     putStrLn $ e3 ++ show r3
     putStrLn $ e4 ++ show r4
     putStrLn $ e5 ++ show r5
     mapM_ f ms 
     
     
     
  where f :: MeasureEval -> IO ()
        f (d, b) = putStrLn $ show d ++ ": " ++ show b
  


type MeasureEval = (EvidenceDescriptor, Bool)
type Demo2EvalResult = (Bool, Bool, Bool, Bool, Bool, [MeasureEval])

evaluate :: Request -> Response -> Demo2EvalResult
evaluate (d, tReq, nonce) 
  ((e, eNonce, eSig), (tpmQuote@((pcrs, qNonce), qSig), hashIn, qpSig)) = 
  let pcrs' = pcrSelect tReq
      tpmBlob = tPack (pcrs, qNonce)
      eBlob = ePack e eNonce
      qBlob = qPack tpmQuote hashIn
      r1 = verify md5 pub qBlob qpSig 
      r2 = verify md5 pub eBlob eSig
      r3 = verify md5 pub tpmBlob qSig 
      r4 = ((doHash eBlob) == hashIn)
      r5 = (nonce == eNonce)
      ms =  evaluateEvidence d e in
 (r1, r2, r3, r4, r5, ms)
  
                                            
evaluateEvidence :: DesiredEvidence -> Evidence -> [MeasureEval]
evaluateEvidence ds es = zipWith f ds es 
 where 
   f :: EvidenceDescriptor -> EvidencePiece -> MeasureEval
   f ed ep = case ed of 
     D0 -> let res = check 0 ep in
       (D0, res)
     D1 -> let res = check 1 ep in
       (D1, res)
     D2 -> let res = check 2 ep in
       (D2, res)
     
       
       
     
check :: Int -> EvidencePiece -> Bool
check id ep = let expected = M.lookup id goldenMap in
                          case expected of 
                            Nothing -> throw $ ErrorCall (g1 ++ show id)
                            Just goldEp -> goldEp == ep 
                         
                         
                          
g1 :: String
g1 = "No Golden Value for measurement #"
                         


expectedEvidence :: Evidence
expectedEvidence = [M0 (B.cons (bit 0) empty), M1 empty, M2 empty]
  
goldenMap = M.fromList $ zip [0..2] expectedEvidence
                                             
e1 :: String
e1 = "Quote Package Signature: "
        
e2 :: String
e2 = "Evidence Package Signature: "  

e3 :: String
e3 = "TPM Signature: "  

e4 :: String
e4 = "Integrity of evidence package: "  

e5 :: String
e5 = "Nonce verified: "

e6 :: String
e6 = "Measurement #"

 
        
        
        
-- PCR primitives
pcrs :: [PCR]
pcrs = correct --wrong
  where correct :: [PCR]
        correct = map bit [0..7]

        wrong :: [PCR]
        --wrong = [(bit 3)] ++ (map bit [1..7])
        wrong = (map bit [0..6]) ++ [(bit 7)]

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





--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"
