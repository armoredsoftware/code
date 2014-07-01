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
import Data.ByteString (empty, ByteString, pack, append)
import Data.Word
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

{-
instance Binary PublicKey where
  put (PublicKey size n e)         = do put size 
                                        put n
                                        put e 

  get = do size <- get :: Get Int
           n <- get :: Get Integer
           e <- get :: Get Integer
           return (PublicKey size n e)
-}

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

getPriKey :: PrivateKey
getPriKey = read "PrivateKey {private_pub = PublicKey {public_size = 255, public_n = 102286069932676439648989610886255086928576415518262989979373822015820473373298917309733169334894078663465654813864795992449002045611618216772107694197239257596906542050824783238423207486620502143852233549323662152913901003839469666106229002333522968796158867719639428162864654318447376554421419853368463460816825839927433535917321040438586195662077833617329460445116174171898844177231110696685109366473693437397141808391998779551680848144607092586071677413344299799355118323107087687451233061481752344137717053392877992158280177371084919091236794566182554721486704343862690413716546638718184408877205483804028378127, public_e = 3}, private_d = 68190713288450959765993073924170057952384277012175326652915881343880315582199278206488779556596052442310436542576530661632668030407745477848071796131492838397937694700549855492282138324413668095901489032882441435275934002559646444070819334889015312530772578479759618775243102878964917702947613235578975640441595530532964860865290538300129362043420523670124435359309089812650198575150810551757831144576650469476635406298808304937863806824095404546393553382411646788097997875007227432328172991809444812353981075423670277961045758251477211819000646275930882291662268129904557838913755947940831225765537790018187203707, private_p = 665200213052143854051101692740663391945842931973138765926093861727860029902325046608097602130381917859727341672657906618276521406824938881802552224833915397580496901252069963309214176731959329032193319938304827265317719042959702339379121504087060767624349832851213176508177560178702596218330045960035980011, private_q = 153767343914934100765334131295651489205001205180169668640226445591195686284602569822440265047478335815322461357271128415526608616501639046884678795114892914219627624609344176575649759397035625796574552120319067747951393820950909399023356703648199170516368952316154640478837735156628234974010568752816711592557, private_dP = 443466808701429236034067795160442261297228621315425843950729241151906686601550031072065068086921278573151561115105271078851014271216625921201701483222610265053664600834713308872809451154639552688128879958869884843545146028639801559586081002724707178416233221900808784338785040119135064145553363973357320007, private_dQ = 102511562609956067176889420863767659470000803453446445760150963727463790856401713214960176698318890543548307571514085610351072411001092697923119196743261942813085083072896117717099839598023750531049701413546045165300929213967272932682237802432132780344245968210769760319225156771085489982673712501877807728371, private_qinv = 229229650308128036416589965682953042325295233606139110064266073549817430885576138694026989939471278971045921961086695667432441308972659041116940577453950615328298216250250390642526209148342637261841598109089159613198128638657809489141457652619039216065888680706401950806298644806729046618032501753659876548}"

{-
getPubKey :: PublicKey
getPubKey = read "PublicKey {public_size = 255, public_n = 102286069932676439648989610886255086928576415518262989979373822015820473373298917309733169334894078663465654813864795992449002045611618216772107694197239257596906542050824783238423207486620502143852233549323662152913901003839469666106229002333522968796158867719639428162864654318447376554421419853368463460816825839927433535917321040438586195662077833617329460445116174171898844177231110696685109366473693437397141808391998779551680848144607092586071677413344299799355118323107087687451233061481752344137717053392877992158280177371084919091236794566182554721486704343862690413716546638718184408877205483804028378127, public_e = 3}"
-}

-- Appraisal primitives
{-
mkHCRequest :: [Int] -> Shared
mkHCRequest mask = let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask
                    in Appraisal(mask', pack [3]) 
-}

mkRequest :: [Int]-> SystemRNG-> Shared
mkRequest mask gen =
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask 
     in Appraisal (mask', fst $ cprgGenerate 16 gen)

mkSignedQuote :: PrivateKey -> Shared -> Shared
mkSignedQuote pri (Appraisal (mask, nonce)) =
    let pcrs' = pcrSelect mask
        quote = (pcrs', nonce) in
        --case sign Nothing md5 pri $ pack' quote of
        --   Left err ->  error $ show err
        --   Right signature -> Attestation (quote, signature)
      Attestation (quote, empty)
{-
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
-}

evaluate :: Shared
evaluate = Result True
