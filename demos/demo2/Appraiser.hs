{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.ByteString (ByteString, pack, append)
import Data.Word
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Binary

data Shared = Appraisal Request
              | Attestation Quote
              | Result Bool


instance Show Shared where
    show (Appraisal app) = "Appraisal: " ++ (show app)
    show (Attestation att) = "Attestation: " ++ (show att)
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."

-- Primitive types
type PCR = Word8
type Mask = Word8
type Nonce = ByteString
type Signature = ByteString
type Request = (Mask, Nonce)
type Quote = (([PCR], Nonce), Signature)

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










prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to Appraise?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop







main :: IO ()
main = let req = mkRequest [0..7] in
  do chan <- sendRequest req
     quote <- receiveQuote chan
     case evaluate req quote of True -> putStrLn "Appraisal Succeeded"
     return ()

                      


   
                                                                

-- Appraisal primitives
mkRequest :: [Int] -> Request
mkRequest mask =
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

receiveQuote :: LibXenVChan -> IO Quote
receiveQuote chan =  do
             ctrlWait chan
             res :: Shared <- receive chan
             case res of 
                                 Attestation quote ->  do
                                                                putStrLn $ "\n" ++ "Appraiser Received: "
                                                                                               ++ (show res)++ "\n"
                                                                return quote
                                 otherwise ->  throw $ ErrorCall quoteReceiveError 
                                                            -- TODO:  error handling here?

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

pcrSelect :: Mask -> [PCR]
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





--Error messages(for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"
