module Conso.Fr.Elec.Rfiles.DecryptRFiles
  ( DecryptConfig(..)
  , decryptZipFile
  , decryptDir
  ) where

import           Crypto.Cipher.AES   (AES256, AES128)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV,
                                      cipherInit, cbcDecrypt)
import           Crypto.Error        (CryptoFailable(..))
import qualified Data.ByteString     as BS
import           Data.Char           (isDigit)
import           Data.List           (isSuffixOf, isPrefixOf, tails)
import           Data.Maybe          (fromMaybe)
import           Control.Monad       (when)
import           System.Directory    (listDirectory, doesDirectoryExist, removeFile)
import           System.Exit         (ExitCode(..))
import           System.FilePath     ((</>), takeFileName, takeDirectory)
import           System.Process      (readProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Types publics
-- ---------------------------------------------------------------------------

data DecryptConfig = DecryptConfig
    { dc128Key     :: Maybe BS.ByteString   -- clé AES-128 (16 bytes)
    , dc128IV      :: Maybe BS.ByteString   -- IV statique  (16 bytes)
    , dc256Key     :: Maybe BS.ByteString   -- clé AES-256 (32 bytes)
    , dcSwitchDate :: Maybe String          -- YYYYMMDD
    }

-- ---------------------------------------------------------------------------
-- Types internes
-- ---------------------------------------------------------------------------

data DecryptMode
    = Mode128 BS.ByteString BS.ByteString  -- key, iv statique
    | Mode256 BS.ByteString                -- key (iv = premiers 16 octets)

-- ---------------------------------------------------------------------------
-- Helpers internes
-- ---------------------------------------------------------------------------

-- | Extrait YYYYMMDD du nom de fichier (premier bloc 8 chiffres débutant par "20")
extractDate :: FilePath -> Maybe String
extractDate fp = go (tails (takeFileName fp))
  where
    go [] = Nothing
    go (s:rest)
      | length s >= 8
      , let cand = take 8 s
      , all isDigit cand
      , "20" `isPrefixOf` cand = Just cand
      | otherwise = go rest

-- | Choisit le mode selon la date du fichier et la config
modeForFile :: DecryptConfig -> FilePath -> Either String DecryptMode
modeForFile cfg fp =
    let useAes128 = case (dc128Key cfg, dc128IV cfg) of
            (Just k, Just iv) -> Right (Mode128 k iv)
            _ -> Left "AES-128 non configuré (zipAes128Key/zipAes128IV manquants)"
        useAes256 = case dc256Key cfg of
            Just k  -> Right (Mode256 k)
            Nothing -> Left "AES-256 non configuré (zipAes256Key manquant)"
    in case dcSwitchDate cfg of
        Nothing         -> useAes128
        Just switchDate ->
            let fileDate = fromMaybe "" (extractDate (takeFileName fp))
            in if fileDate < switchDate
               then useAes128
               else useAes256

-- ---------------------------------------------------------------------------
-- Déchiffrement
-- ---------------------------------------------------------------------------

-- | AES-256-CBC, IV = 16 premiers octets du fichier
decryptCBC256 :: BS.ByteString -> BS.ByteString -> Either String BS.ByteString
decryptCBC256 keyBytes ciphertext
    | BS.length ciphertext < 16 = Left "Fichier trop court (< 16 octets)"
    | otherwise =
        let (ivBytes, payload) = BS.splitAt 16 ciphertext
        in case makeIV ivBytes :: Maybe (IV AES256) of
            Nothing -> Left "IV invalide"
            Just iv -> case cipherInit keyBytes of
                CryptoFailed err -> Left (show err)
                CryptoPassed cipher ->
                    Right $ removePadding (cbcDecrypt (cipher :: AES256) iv payload)

-- | AES-128-CBC, IV statique fourni explicitement
decryptCBC128 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
decryptCBC128 keyBytes ivBytes ciphertext =
    case makeIV ivBytes :: Maybe (IV AES128) of
        Nothing -> Left "IV AES-128 invalide"
        Just iv -> case cipherInit keyBytes of
            CryptoFailed err -> Left (show err)
            CryptoPassed cipher ->
                Right $ removePadding (cbcDecrypt (cipher :: AES128) iv ciphertext)

-- | Supprime le padding PKCS#7
removePadding :: BS.ByteString -> BS.ByteString
removePadding bs
    | BS.null bs = bs
    | otherwise  =
        let padLen = fromIntegral (BS.last bs)
        in BS.dropEnd padLen bs

-- ---------------------------------------------------------------------------
-- API publique
-- ---------------------------------------------------------------------------

-- | Déchiffre un fichier .zip sur disque (écrase avec le contenu déchiffré)
decryptZipFile :: DecryptMode -> FilePath -> IO (Either String ())
decryptZipFile mode path = do
    raw <- BS.readFile path
    case mode of
        Mode128 key iv ->
            case decryptCBC128 key iv raw of
                Left err -> return (Left err)
                Right pt -> BS.writeFile path pt >> return (Right ())
        Mode256 key ->
            case decryptCBC256 key raw of
                Left err -> return (Left err)
                Right pt -> BS.writeFile path pt >> return (Right ())

-- | Déchiffre tous les .zip d'un répertoire (récursif)
decryptDir :: DecryptConfig -> FilePath -> IO ()
decryptDir cfg dir = do
    putStrLn $ "Répertoire : " <> dir
    entries <- listDirectory dir
    mapM_ (processEntry cfg dir) entries

processEntry :: DecryptConfig -> FilePath -> FilePath -> IO ()
processEntry cfg dir name = do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
        then decryptDir cfg path
        else when (".zip" `isSuffixOf` name)
                $ do putStr $ "  Déchiffrement : " <> name <> " ... "
                     case modeForFile cfg path of
                       Left err -> putStrLn $ "ERREUR (config) : " <> err
                       Right mode
                         -> do res <- decryptZipFile mode path
                               case res of
                                 Left err -> putStrLn $ "ERREUR : " <> err
                                 Right () -> do
                                     let outDir = takeDirectory path
                                     (code, _, err) <- readProcessWithExitCode "unzip" ["-o", path, "-d", outDir] ""
                                     case code of
                                         ExitSuccess   -> removeFile path >> putStrLn "OK"
                                         ExitFailure n -> putStrLn $ "ERREUR unzip (code " <> show n <> ") : " <> err
