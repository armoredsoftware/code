{- ECA -- It's good practice to always use a module name and always use an
          explicit list of exports.  Module names need to match file names, and
          must start with an uppercase letter, FYI.
-} 
module Demo1V1 where

import qualified Data.Word as W

{- ECA -- This is what you had.
import Data.Bits ((.|.))
import Data.Bits

Note that an unqualified, i.e. full, import of Data.Bits will include the
operator, so just "import Data.Bits" is sufficient for what you want.
-}
import Data.Bits

{- ECA - you were missing the "qualified" keyword here.  
         That's why you were getting name conflicts.
-}
import qualified Data.ByteString as B
{- Note that you can pair qualified imports with unqualified, limited imports.
   This will let you use non-conflicting names, like append and pack, without
   having to qualify them with B.
-}
import Data.ByteString (append, pack)

import Crypto.Nonce
import Data.Binary
import Data.Maybe
{- ECA - Be wary of importing libraries like this.  
         For one, the library has been deprecated in favor of a newer one 
         (either).  Secondly, you're adding a non-standard dependency for a
         single function that you could right yourself, or do via pattern
         matching.
-}
--import Data.Either.Unwrap

--import Crypto.Cipher.RSA
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Crypto.PubKey.HashDescr
import Crypto.Random

-- ECA - my imports
import Control.Monad

{- ECA -- General Notes:

Re-using other people's libraries can be great, but be aware of what they are
doing.  For example, the crypto-nonce library you use relies on a different
source of entropy than the crypto-random library you use (SystemRNG vs. AESRNG).
This could be good or bad depending on how we want to design our system, so it
should be documented.

If we want to reuse SystemRNG instead of the crypto-nonce library, we can
generate nonces via > let (nonce, _) = cprgGenerate 16 g, using the generator,
g, that you created in the main function.

I've included how I would write everything at the bottom if you want a look
at a different style.
-}


{- ECA -- annotating types for functions is almost always a good idea, even for
          obvious functions like main.
-}      
main :: IO ()
main = do e <- createEntropyPool
          req <- (generateRequest . generateDesired) [0..7]
          let g = (cprgCreate e) :: SystemRNG
              {- ECA -- what you had: ((pub, pri), g') = generate g 255 3
                 You don't use the next generator, g', so you're better off
                 not binding a name for it.  This cleans up the namespace and
                 helps the compiler out.  If you compile with -Wall, you'll get
                 a warning about this, FYI. -}
              ((pub, pri), _) = generate g 255 3
              result = doAppraisal req appraiserExpectedPCRcontents (pub,pri)
          print result
          {- ECA -- The type of print is IO (), so you don't need a return 
                    statement after it.
          return ()
          -}



-------------------------------------------------------------------------------------------------------------------------------------------
--DATA TYPES

--types for the measurements we are interested in on the target machine(list of PCR values)
type PCRval = W.Word8
type PCRlist = [PCRval]





type D = W.Word8 --type for the bitmap that specifies desired PCR registers.


type PCRcontents = (PCRlist, D)   {-type to package the PCR list along with the bitmap that specifies which PCR values are included in the list.
  Note:  PCRlist holds PCR values in increasing positional order (positions included are specified by accompanying bitmap(D)), 
         D(bitmap) specifies PCR presence(i.e. the least significant bit represents PCR 0, second-least significant is PCR 1, etc.) 
-}



type Nonce = B.ByteString
--type PriKey = B.ByteString  -- only available to target
type PubKey = B.ByteString -- appraiser has this


type Request = (D, Nonce) --type for the request packet sent from appraiser to attestation agent

type Quote = (PCRcontents, Nonce) --type for the data packet sent from attestation agent to the appraiser in response to a request

type Signature = B.ByteString
type SignedQuote = (Quote, Signature)




-------------------------------------------------------------------------------------------------------------------------------------------
--APPRAISER ACTIONS


-- ECA - try not to leave old code commented out like this, it can be confusing.
--(pubKey, priKey) = fromJust $ generateWith (2, 5) 255 3 


{- ECA - total nitpicking, but try and keep code to 80 horizontal spaces.
         It helps readability and forces you to use newlines and spacing to
         separate code bodies.
What you had:
doAppraisal :: Request -> PCRcontents -> (PublicKey, PrivateKey) -> Bool
doAppraisal request@(_, requestNonce) appraiserExpected (pubKey, priKey) = let  (quote@(pcrCon, quoteNonce), signature) = getSignedQuote request priKey
                                                                                a = verify md5 pubKey (packQuote quote {-(alteredPCRcontent, quoteNonce)-}) signature --TODO key handling
                                                                                b = requestNonce == quoteNonce
                                                                                c = pcrCon == appraiserExpected
                                                                             in and [a,b,c]
-}
doAppraisal :: Request -> PCRcontents -> (PublicKey, PrivateKey) -> Bool
doAppraisal request@(_, requestNonce) appraiserExpected (pubKey, priKey) = 
    let (quote@(pcrCon, quoteNonce), signature) = getSignedQuote request priKey
        a = verify md5 pubKey (packQuote quote) signature
        b = requestNonce == quoteNonce
        c = pcrCon == appraiserExpected in 
      and [a,b,c]

-- ECA: leaving this uncommented won't hurt things as long as it's not called.
verifySignature :: HashDescr -> PubKey -> Quote -> Signature -> Bool
verifySignature = undefined 



--Generates 128 bit nonce as a ByteString of 16 bytes.  TODO:  may want to generate nonce "under the hood"
generateNonce :: IO B.ByteString
generateNonce = do g <- new
                   nonce128 g
{- ECA: you could have alternatively written this as > nonce128 =<< new
        or > new >>= nonce128

        Check out the operators in Control.Monad to see what other space saving
        techniques there are.  Some are pretty useful for simple pieces of code
        like this.
-}


--takes a list specifying desired PCR registers, returns a request packet
generateRequest :: D -> IO Request
generateRequest d =  do b <- generateNonce 
                        return (d, b) 






--helper function for testing.  Builds the bitmap.  1 means PCR is desired, 0 otherwise(Examples using W.Word8:  generateDesired [0,1] === 00000011,   generateDesired [2,4] === 00010100

{- ECA -- Instead of qualifing Prelude names, it's better to import any
          conflicting packages with an alias.

What you had:
generateDesired :: [Int] -> D
generateDesired xs = let a = Prelude.map (bit) xs
                     in Prelude.foldr (.|.) zeroBits a
-}
generateDesired :: [Int] -> D
generateDesired xs = 
    let a = map (bit) xs in 
      foldr (.|.) zeroBits a

-----------------------------------------------------------------------------------------------------------------------------
--TARGET ACTIONS

-- ECA - definitely want types for non-standard functions/values.
md5 :: HashDescr
md5 = hashDescrMD5

--this function simulates action that takes palce on the remote target.
--takes request as input from appraiser and returns a quote that contains PCR contents of the target and the unique nonce from the request.
generateQuote :: Request -> Quote
generateQuote (requested, nonce) = 
     ((gatherTargetPCRlist requested, requested), nonce) -- TODO sign with key


getSignedQuote :: Request -> PrivateKey -> SignedQuote
getSignedQuote request priKey = 
    let a = generateQuote request --generateQuote request
        {- ECA -- be very careful when using functions like fromRight,
                  especially when used in conjunction with a function that might
                  throw an error!  Better to explicitly case over the result.
        What you had:
        b = fromRight $ getQuoteSignature a priKey
        -}
        b = case getQuoteSignature a priKey of
              Right res -> res
              Left err -> error $ show err in 
        {- Alternatively, if you know for sure the value is a Right value,
           pattern matching is usually cleaner than using fromRight or
           case:
           
        (Right b) = getQuoteSignature a priKey
        -}
      (a,b)
--sendQuote :: Quote -> IO ()


getQuoteSignature :: Quote -> PrivateKey -> Either Error B.ByteString
getQuoteSignature quote priKey = 
    sign Nothing md5 priKey (packQuote quote)  --use Data.Binary to encode



packQuote :: Quote -> B.ByteString
packQuote ((pcrList, d), nonce) = 
    append (pack (pcrList ++ [d])) nonce

















-------------------------------------------------------------------------------------------------------------------------------------------
-- TEST FUNCTIONS(TODO:  use QuickCheck instead, or in addition to these)

pcrLength :: Int
pcrLength = 8

iterateList = [0..(pcrLength - 1)]

--gathers target PCR list based on bitmap input(desired registers).  
--TODO:  this function will eventually need to interact with Measurement mechanisms on the target (rather than using our dummy iterate' function with static targetPCRlist).
gatherTargetPCRlist :: D -> PCRlist
gatherTargetPCRlist d = iterate' iterateList d targetPCRlist 

-- ECA -- added type
iterate' :: [Int] -> D -> PCRlist -> PCRlist
iterate' xs d pcrList = iterate'' xs d pcrList []

--takes list of integers to simulate loop counter (accessing bits 0..(pcrLength - 1)). takes bitmap of desired PCRs, PCRlist for target, and also an accumulator PCRlist.  Returns a PCRlist with the PCRs specified in the bitmap.  We may not need this on the target system?
iterate'' :: [Int] -> D -> PCRlist -> PCRlist -> PCRlist
iterate'' [] _ _ acc = acc
iterate'' (x:xs) d pcrList acc = let a = testBit d x
                                     b = pcrList !! x
                                 in if a then iterate'' xs d pcrList (acc ++ [b])
                                    else iterate'' xs d pcrList acc



-------------------------------------------------------------------------------------------------------------------------------------------
--STATIC DATA

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


--targetPrivateKey :: PriKey
--targetPrivateKey = zeroBits  --hardcode?

--targetPublicKey :: PubKey
--targetPublicKey  = zeroBits  --hardcode?



-------------------------------------------------------------------------------------------------------------------------------------------
--Extra code

{-
appraiseQuote :: Quote -> Bool
appraiseQuote (x@(pcr, requested), nonce, key) = let a = (x `compare'` targetPCRlist)
                                                     b = (True)  -- TODO get original nonce
                                                     c = (True)  -- TODO key
                                                 in and[a,b,c]
-}


{-
validate :: PCRcontents -> PCRlist -> Bool
validate (pcr, requested) local = pcr == (iterate' iterateList requested local [])
-}


{- ECA -- Alternate version -}
{- I think your definition of a signed quote is wrong.  We don't need to include
   the mask in our quote, just the pcrlist, nonce, and signature.
-}
type SignedQuote' = ((PCRlist, Nonce), Signature)

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
mkSignedQuote :: Request -> PCRlist -> PrivateKey -> Either MyError SignedQuote'
mkSignedQuote (mask, nonce) pcrs pri =
    let pcrs' = pcrSelect pcrs mask
        quote = (pcrs', nonce) in
      do signature <- liftErr $ sign Nothing md5 pri $ pack' quote
         return (quote, signature)

{- Evaluate a provided quote.  Again, we propogate errors. -}
evaluate :: Request -> SignedQuote' -> PCRlist -> PublicKey 
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
        
