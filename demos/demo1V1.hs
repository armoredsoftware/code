module Demo1V1 where

import qualified Data.Word as W
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString (append, pack)


import Data.Binary
import Data.Maybe

import Crypto.Nonce
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Crypto.PubKey.HashDescr
import Crypto.Random

import Control.Monad

      




-------------------------------------------------------------------------------------------------------------------------------------------
--DATA TYPES

--types for the measurements we are interested in on the target machine(list of PCR values)
type PCRval = W.Word8
type PCRlist = [PCRval]--PCRlist holds PCR values in increasing positional order
type D = W.Word8 --type for the bitmap that specifies desired PCR registers. (least significant bit represents PCR 0, second-least significant is PCR 1, etc.) 


type Nonce = B.ByteString
type Request = (D, Nonce) --type for the request packet sent from appraiser to attestation agent

type Signature = B.ByteString
type SignedQuote = ((PCRlist, Nonce), Signature)

-- ECA - definitely want types for non-standard functions/values.
md5 :: HashDescr
md5 = hashDescrMD5


{- ECA -- Alternate version -}

-- This is to handle custom errors, in addition to crypto errors.
data MyError = AppraisalError String | CryptoError Error deriving Show
liftErr :: Either Error a -> Either MyError a
liftErr (Left err) = Left $ CryptoError err
liftErr (Right res) = Right res

main2 :: IO ()
main2 = 
    -- The only thing we need the IO monad for is entropy and printing (for now)
    do g <- liftM cprgCreate createEntropyPool
       print $ case doAppraisal2 g of
                 Right True -> "Appraisal succeeded."
                 Right False -> "Appraisal failed."
                 Left err -> "Error: " ++ show err

{- Think of an appraisal as the three step process we've talked about:
   1)  Send a request.
   2)  Receive a quote.
   3)  Evaluate.

   For a shallow embedding, each step should be it's own function, for clarity.
   Note that we use the Either monad to propogate errors.
-}
doAppraisal2 :: SystemRNG -> Either MyError Bool
doAppraisal2 gen =
    let req = mkRequest [0..7] gen' in
      do quote <- mkSignedQuote req pcrs pri
         evaluate req quote pcrs pub
  where {- We setup the keys, pcrs, and next generator here, under the 
           assumption that they're globally known knowledge. We can parameterize
           later as needed. -}
        pub :: PublicKey
        pri :: PrivateKey
        gen' :: SystemRNG
        ((pub, pri), gen') = generate gen 255 3

        pcrs :: PCRlist
        pcrs = map bit [0..7]

-- new version of pack for my modified quote
pack' :: (PCRlist, Nonce) -> B.ByteString
pack' (pcrs, nonce) = pack pcrs `append` nonce

{- This is my version of your gatherTargetPCRlist function.  More efficient
   since the list comprehension will be fused down to one traversal.

   The fusion will be an efficient version of:
   map fst (map (\ (_, n) -> testBit mask n) (zip pcrs [0..7]))

   Basically, we're zipping the pcr list with indexes and using those indexes
   to test if a bit is set in our mask.
-}
pcrSelect :: PCRlist -> D -> PCRlist
pcrSelect pcrs mask =
    [ x | x <- pcrs, n <- [0..7], testBit mask n]

-- Make our request using a parameterized generator to make the nonce.
mkRequest :: [Int] -> SystemRNG -> Request
mkRequest mask gen = 
    {- This is my version of your generateDesired function.  Slightly more
       efficient since we only navigate our list once. -}
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask in
      (mask', fst $ cprgGenerate 16 gen)

{- Make our quote using a request and list of pcrs.  Note that we propogate
   errors rather than discarding them.
-}
mkSignedQuote :: Request -> PCRlist -> PrivateKey -> Either MyError SignedQuote
mkSignedQuote (mask, nonce) pcrs pri =
    let pcrs' = pcrSelect pcrs mask
        quote = (pcrs', nonce) in
      do signature <- liftErr $ sign Nothing md5 pri $ pack' quote
         return (quote, signature)

{- Evaluate a provided quote.  Again, we propogate errors. -}
evaluate :: Request -> SignedQuote -> PCRlist -> PublicKey 
         -> Either MyError Bool
evaluate (mask, rnonce) (quote@(qpcrs, qnonce), signature) pcrs pub =
    let pcrs' = pcrSelect pcrs mask in
      if not $ verify md5 pub (pack' quote) signature
      then Left $ AppraisalError "Signature could not be verified."
      else if rnonce /= qnonce
           then Left $ AppraisalError "Nonce could not be verified."
           else if pcrs' /= qpcrs
                then Left $ AppraisalError "PCR not of expected value."
                else return True
        









-------------------------------------------------------------------------------------------------------------------------------------------
--STATIC DATA

{-
alteredPCRcontent :: PCRcontents
alteredPCRcontent = ([bit 0, bit 1, bit 2, bit 3, bit 4, bit 5, bit 6, bit 7], generateDesired [0..7])

--static PCR list on target(this is just for testing--once we implement measurement, we will get this from target system)
targetPCRlist :: PCRlist
--targetPCRlist = [empty]--[bit 0, bit 1, bit 2, bit 3, bit 4, bit 5, bit 6, bit 7]
targetPCRlist = [bit 0, bit 1, bit 2, bit 3, bit 4, bit 5, bit 6, bit 7]

--example of an expected PCR contents that an appraiser might maintain.  
appraiserExpectedPCRcontents :: PCRcontents
--appriaserExpectedPCRcontents = ([empty], generateDesired [0..(pcrLength - 1)]) --([bit 0, bit 1, bit 2, bit 3, bit 4, bit 5, bit 6, bit 7], generateDesired [0..(pcrLength - 1)])
appraiserExpectedPCRcontents = ([bit 0, bit 1, bit 2, bit 3, bit 4, bit 5, bit 6, bit 7], generateDesired [0..(pcrLength - 1)])
-}
