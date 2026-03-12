module Conso.Fr.Elec.Rfiles.DecryptRFiles
  ( decryptZipFile
  , decryptDir
  ) where

import           Crypto.Cipher.AES   (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV,
                                      cipherInit, cbcDecrypt)
import           Crypto.Error        (CryptoFailable(..))
import qualified Data.ByteString     as BS
import           Data.ByteArray.Encoding (convertFromBase, Base(..))
import           Data.List           (isSuffixOf)
import           System.Directory    (listDirectory, doesDirectoryExist)
import           System.FilePath     ((</>))

-- | Déchiffre un ByteString AES-256-CBC (IV = 16 premiers octets)
decryptCBC :: BS.ByteString -> BS.ByteString -> Either String BS.ByteString
decryptCBC keyBytes ciphertext
    | BS.length ciphertext < 16 = Left "Fichier trop court (< 16 octets)"
    | otherwise =
        let (ivBytes, payload) = BS.splitAt 16 ciphertext
        in case makeIV ivBytes :: Maybe (IV AES256) of
            Nothing -> Left "IV invalide"
            Just iv -> case cipherInit keyBytes of
                CryptoFailed err -> Left (show err)
                CryptoPassed cipher ->
                    Right $ removePadding (cbcDecrypt (cipher :: AES256) iv payload)

-- | Supprime le padding PKCS#7
removePadding :: BS.ByteString -> BS.ByteString
removePadding bs
    | BS.null bs = bs
    | otherwise  =
        let padLen = fromIntegral (BS.last bs)
        in BS.dropEnd padLen bs

-- | Déchiffre un fichier .zip sur disque (écrase avec le contenu déchiffré)
decryptZipFile :: String -> FilePath -> IO (Either String ())
decryptZipFile password path = do
    case convertFromBase Base16 (BS.pack (map (fromIntegral . fromEnum) password)) of
        Left err       -> return (Left $ "Clé AES invalide (hex attendu) : " <> err)
        Right keyBytes -> do
            raw <- BS.readFile path
            case decryptCBC keyBytes raw of
                Left err        -> return (Left err)
                Right plaintext -> do
                    BS.writeFile path plaintext
                    return (Right ())

-- | Déchiffre tous les .zip d'un répertoire (récursif)
decryptDir :: String -> FilePath -> IO ()
decryptDir password dir = do
    putStrLn $ "Répertoire : " <> dir
    entries <- listDirectory dir
    mapM_ (processEntry password dir) entries

processEntry :: String -> FilePath -> FilePath -> IO ()
processEntry password dir name = do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
        then decryptDir password path
        else if ".zip" `isSuffixOf` name
            then do
                putStr $ "  Déchiffrement : " <> name <> " ... "
                res <- decryptZipFile password path
                case res of
                    Left err -> putStrLn $ "ERREUR : " <> err
                    Right () -> putStrLn "OK"
            else return ()
