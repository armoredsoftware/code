module Demo1Utils where

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


import Debug.Trace

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
 --             | Key PublicKey


instance Show Shared where
    show (Appraisal req) = "Appraisal: "++(show req)
    show (Attestation quote) = "Attestation: "++(show quote)
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."
 --   show (Key x) = "PublicKey: "++(show x)


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

instance Binary PublicKey where
  put (PublicKey size n e)         = do put size 
                                        put n
                                        put e 

  get = do size<- get :: Get Int
           n <- traceShow size (get :: Get Integer)
           e <- traceShow n (get :: Get Integer)
           () <- traceShow e (return ())
           return (PublicKey size n e)

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

{-
{-# NOINLINE getKeys #-}
getKeys :: (PrivateKey, PublicKey)
getKeys = unsafePerformIO readKeys

getPriKey :: PrivateKey
getPriKey = fst getKeys

getPubKey :: PublicKey
getPubKey = snd getKeys

--readKeys ::Maybe (PublicKey,PrivateKey)
--readKeys = generateWith (3,7) 255 0x10001
-}

getPriKey :: IO PrivateKey
getPriKey =
     do handle <- openFile "prikey.txt" ReadMode
        priString <- hGetLine handle
        let pri :: PrivateKey
            pri = read priString
        hClose handle
        return pri

getPubKey :: IO PublicKey
getPubKey =
     do handle <- openFile "pubkey.txt" ReadMode
        pubString <- hGetLine handle
        let pub :: PubvateKey
            pub = read pubString
        hClose handle
        return pub

--Utility function to be used ONCE to generate keys and ouput them to keys.txt
exportKeys :: IO ()
exportKeys  =
     do e <- createEntropyPool
        let gen :: SystemRNG
            gen = cprgCreate e
            ((pub, pri), _) = generate gen 255 3
        doExport pri pub
        putStrLn "Created file keys.txt"

--Helper for exportKeys
doExport :: PrivateKey -> PublicKey ->  IO ()
doExport pri pub =
                   do handle <- openFile "keys.txt" WriteMode
                      hPutStrLn handle $ show pri
                      hPutStrLn handle $ show pub
                      hClose handle


-- Appraisal primitives
mkHCRequest :: [Int] -> Shared
mkHCRequest mask = let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask
                    in Appraisal(mask', pack [3]) 

mkRequest :: [Int]-> SystemRNG-> Shared
mkRequest mask gen =
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask 
     in Appraisal (mask', fst $ cprgGenerate 16 gen)

mkSignedQuote :: PrivateKey -> Shared -> Shared
mkSignedQuote pri (Appraisal (mask, nonce)) =
    let pcrs' = pcrSelect mask
        quote = (pcrs', nonce) in
        case sign Nothing md5 pri $ pack' quote of
           Left err ->  error $ show err
           Right signature -> Attestation (quote, signature)

evaluate :: PublicKey -> Shared -> Shared -> Shared
evaluate pub (Appraisal (mask, rnonce)) (Attestation (quote@(qpcrs, qnonce), signature)) =
    let pcrs' = pcrSelect mask in
     if (not $ verify md5 pub (pack' quote) signature)then 
              error "Signature could not be verified."
     else
       if (rnonce /= qnonce)then
              error "Nonce could not be verified."
       else
         if (pcrs' /= qpcrs) then
              error "PCR not of expected value."
         else
           Result True
