import System.IO

-- crypto libraries
import Crypto.Random
import Crypto.PubKey.HashDescr
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15


main :: IO ()
main = do
  putStrLn "Provide file name where keys will be exported:"
  fileName <- getLine
  exportKeys fileName
  putStrLn $ "Created file: " ++ fileName 



--Utility function to be used ONCE to generate keys and ouput them to keys.txt
exportKeys :: String -> IO ()
exportKeys  fileName =
     do e <- createEntropyPool
        let gen :: SystemRNG
            gen = cprgCreate e
            ((pub, pri), _) = generate gen 255 3
        doExport fileName pri pub
        

--Helper for exportKeys
doExport :: String -> PrivateKey -> PublicKey ->  IO ()
doExport fileName pri pub =
                   do handle <- openFile fileName WriteMode
                      hPutStrLn handle $ show pri
                      hPutStrLn handle $ show pub
                      hClose handle