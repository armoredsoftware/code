import qualified Data.Word as W
import Data.Bits ((.|.))
import Data.Bits
import Data.ByteString as B
import Crypto.Nonce
import Data.Binary
import Data.Maybe
import Data.Either.Unwrap

--import Crypto.Cipher.RSA
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Crypto.PubKey.HashDescr
import Crypto.Random


main = do e <- createEntropyPool
          n <- generateNonce
          let g = (cprgCreate e) :: SystemRNG
              ((pub, pri), g') = generate g 255 3
              req = (generateDesired [0..7], n) :: Request
              result = doAppraisal req appraiserExpectedPCRcontents (pub,pri)
          print result
          return ()



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
--type PubKey = B.ByteString -- appraiser has this


type Request = (D, Nonce) --type for the request packet sent from appraiser to attestation agent

type Quote = (PCRcontents, Nonce) --type for the data packet sent from attestation agent to the appraiser in response to a request

type Signature = B.ByteString
type SignedQuote = (Quote, Signature)




-------------------------------------------------------------------------------------------------------------------------------------------
--APPRAISER ACTIONS


--(pubKey, priKey) = fromJust $ generateWith (2, 5) 255 3 



doAppraisal :: Request -> PCRcontents -> (PublicKey, PrivateKey) -> Bool
doAppraisal request@(_, requestNonce) appraiserExpected (pubKey, priKey) = let  (quote@(pcrCon, quoteNonce), signature) = getSignedQuote request priKey
                                                                                a = verify md5 pubKey (packQuote quote) {-(alteredPCRcontent, quoteNonce))-} signature --TODO key handling
                                                                                b = requestNonce == quoteNonce
                                                                                c = pcrCon == appraiserExpected
                                                                             in and [a,b,c]


--verifySignature :: HashDescr -> PubKey -> Quote -> Signature -> Bool
--verifySignature = undefined 



--Generates 128 bit nonce as a ByteString of 16 bytes.  TODO:  may want to generate nonce "under the hood"
generateNonce :: IO B.ByteString
generateNonce = do g <- new
                   nonce128 g


--takes a list specifying desired PCR registers, returns a request packet
generateRequest :: D -> IO Request
generateRequest d =  do b <- generateNonce 
                        return (d, b) 






--helper function for testing.  Builds the bitmap.  1 means PCR is desired, 0 otherwise(Examples using W.Word8:  generateDesired [0,1] === 00000011,   generateDesired [2,4] === 00010100
generateDesired :: [Int] -> D
generateDesired xs = let a = Prelude.map (bit) xs
                     in Prelude.foldr (.|.) zeroBits a


-----------------------------------------------------------------------------------------------------------------------------
--TARGET ACTIONS

md5 = hashDescrMD5

--this function simulates action that takes palce on the remote target.
--takes request as input from appraiser and returns a quote that contains PCR contents of the target and the unique nonce from the request.
generateQuote :: Request -> Quote
generateQuote (requested, nonce) = ((gatherTargetPCRlist requested, requested), nonce) -- TODO sign with key


getSignedQuote :: Request -> PrivateKey -> SignedQuote
getSignedQuote request priKey = let a = generateQuote request --generateQuote request
                                    b = fromRight $ getQuoteSignature a priKey
                                in (a,b)
--sendQuote :: Quote -> IO ()


getQuoteSignature :: Quote -> PrivateKey -> Either Error B.ByteString
getQuoteSignature quote priKey = sign Nothing md5 priKey (packQuote quote)  --use Data.Binary to encode



packQuote :: Quote -> B.ByteString
packQuote ((pcrList, d), nonce) = append (pack (pcrList ++ [d])) nonce

















-------------------------------------------------------------------------------------------------------------------------------------------
-- TEST FUNCTIONS(TODO:  use QuickCheck instead, or in addition to these)

pcrLength :: Int
pcrLength = 8

iterateList = [0..(pcrLength - 1)]

--gathers target PCR list based on bitmap input(desired registers).  
--TODO:  this function will eventually need to interact with Measurement mechanisms on the target (rather than using our dummy iterate' function with static targetPCRlist).
gatherTargetPCRlist :: D -> PCRlist
gatherTargetPCRlist d = iterate' iterateList d targetPCRlist 


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







 
