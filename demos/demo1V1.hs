module Demo1V1 where

-- concurrency libraries
import Control.Concurrent
import Control.Concurrent.STM

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
import System.IO.Unsafe (unsafePerformIO)
import Data.Binary
      
-- Primitive types
type PCR = Word8
type Mask = Word8 
type Nonce = ByteString
type Signature = ByteString
type Request = (Mask, Nonce)
type Quote = (([PCR], Nonce), Signature)

data Shared = Appraisal Request
              | Attestation Quote
              | Result Bool


instance Show Shared where
    show Appraisal{} = "Appraisal"
    show Attestation{} = "Attestation"
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
gen' :: SystemRNG
((pub, pri), gen') = generate gen 255 3

-- Appraisal primitives
mkRequest :: [Int] -> Request
mkRequest mask =
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask in
      (mask', fst $ cprgGenerate 16 gen)

mkSignedQuote :: Request -> STM Quote
mkSignedQuote (mask, nonce) =
    let pcrs' = pcrSelect mask
        quote = (pcrs', nonce) in
      case sign Nothing md5 pri $ pack' quote of
         Left err -> throwSTM . ErrorCall $ show err
         Right signature -> return (quote, signature)

evaluate :: Request -> Quote -> STM Bool
evaluate (mask, rnonce) (quote@(qpcrs, qnonce), signature) =
    let pcrs' = pcrSelect mask in
      do when (not $ verify md5 pub (pack' quote) signature) . throwSTM $
              ErrorCall "Signature could not be verified."
         when (rnonce /= qnonce) . throwSTM $
              ErrorCall "Nonce could not be verified."
         when (pcrs' /= qpcrs) . throwSTM $
              ErrorCall "PCR not of expected value."
         return True

-- The fun stuff
main :: IO ()
main = 
    do m <- newEmptyTMVarIO
       forkIO $ spawnAppraisal m
       forkIO $ spawnAttestation m
       print =<< atomically (getResult m)
  where getResult :: TMVar Shared -> STM Bool
        getResult m  = 
            do v <- readTMVar m
               case v of
                 Result res -> return res
                 otherwise -> retry

{- Think of an appraisal as the three step process we've talked about:
   1)  Send a request.
   2)  Receive a quote.
   3)  Evaluate.

   For a shallow embedding, each step should be it's own function, for clarity.
   Note that we use the Either monad to propogate errors.
-}
spawnAppraisal :: TMVar Shared -> IO ()
spawnAppraisal m =
  let req = mkRequest [0..7] in
    do atomically $ do cond <- tryPutTMVar m $ Appraisal req
                       when (not cond) . throwSTM $ 
                         ErrorCall "Target not ready for request."
       sq <- atomically $ getSignedQuote m
       atomically $ do result <- evaluate req sq
                       void . swapTMVar m $ Result result  
  where getSignedQuote :: TMVar Shared -> STM Quote
        getSignedQuote m  = 
            do v <- readTMVar m
               case v of
                 Attestation sq -> return sq
                 otherwise -> retry


spawnAttestation :: TMVar Shared -> IO ()
spawnAttestation m = atomically $
  do req <- getRequest m
     sq <- mkSignedQuote req
     void . swapTMVar m $ Attestation sq
  where getRequest :: TMVar Shared -> STM Request
        getRequest m = 
            do v <- readTMVar m
               case v of 
                 Appraisal req -> return req
                 otherwise -> retry
