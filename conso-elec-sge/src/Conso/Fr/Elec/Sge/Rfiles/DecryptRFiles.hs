{-|
Module      : Conso.Fr.Elec.Sge.Rfiles.DecryptRFiles
Description : Déchiffrement AES des fichiers Rxx Enedis (AES-128-CBC et AES-256-CBC)

Déchiffre les fichiers @.zip@ téléchargés depuis le serveur SFTP Enedis.

Enedis utilise deux algorithmes selon la date du fichier :

  * Avant 'dcSwitchDate' — AES-128-CBC avec IV statique fourni dans la config
  * À partir de 'dcSwitchDate' — AES-256-CBC avec IV embarqué dans les 16 premiers
    octets du fichier chiffré

La date est extraite du nom de fichier (format @YYYYMMDD@). Le déchiffrement
se fait en place : le fichier @.zip@ chiffré est remplacé par le @.zip@ en
clair, puis décompressé via @unzip@.

Usage :

> cfg  <- getConfig 
> let dcfg = DecryptConfig
>               { dc128Key     = hexToBytes <$> zipAes128Key cfg
>               , dc128IV      = hexToBytes <$> zipAes128IV  cfg
>               , dc256Key     = hexToBytes <$> zipAes256Key cfg
>               , dcSwitchDate = zipAesSwitchDate cfg
>               }
> decryptDir dcfg "/tmp/rfiles"
-}
module Conso.Fr.Elec.Sge.Rfiles.DecryptRFiles
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
import           Data.List           (isSuffixOf, isPrefixOf, isInfixOf, tails)
import           Data.Maybe          (fromMaybe)
import           Control.Monad       (when, forM_)
import           System.Directory    (listDirectory, doesDirectoryExist, removeFile, renameFile, createDirectoryIfMissing)
import           System.Exit         (ExitCode(..))
import           System.FilePath     ((</>), takeFileName, takeDirectory)
import           System.Process      (readProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Types publics
-- ---------------------------------------------------------------------------

-- | Paramètres cryptographiques pour le déchiffrement des fichiers Rxx.
data DecryptConfig = DecryptConfig
    { dc128Key     :: Maybe BS.ByteString
    -- ^ Clé AES-128 (16 octets), convertie depuis la représentation hexadécimale de 'zipAes128Key'.
    , dc128IV      :: Maybe BS.ByteString
    -- ^ IV statique AES-128 (16 octets), converti depuis 'zipAes128IV'.
    , dc256Key     :: Maybe BS.ByteString
    -- ^ Clé AES-256 (32 octets), convertie depuis la représentation hexadécimale de 'zipAes256Key'.
    , dcSwitchDate :: Maybe String
    -- ^ Date de bascule AES-128 → AES-256, format @YYYYMMDD@.
    --   Les fichiers dont la date (dans le nom) est antérieure à cette valeur
    --   sont déchiffrés en AES-128 ; les autres en AES-256.
    --   @Nothing@ = tout AES-128.
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

-- | Choisit le mode selon le répertoire et la config.
--   Les fichiers sous un répertoire "fluxr" (publication périodique Enedis) utilisent
--   toujours AES-256-CBC. Les autres (M023) utilisent AES-128-CBC ou la date de bascule.
modeForFile :: DecryptConfig -> FilePath -> Either String DecryptMode
modeForFile cfg fp =
    let useAes128 = case (dc128Key cfg, dc128IV cfg) of
            (Just k, Just iv) -> Right (Mode128 k iv)
            _ -> Left "AES-128 non configuré (zipAes128Key/zipAes128IV manquants)"
        useAes256 = case dc256Key cfg of
            Just k  -> Right (Mode256 k)
            Nothing -> Left "AES-256 non configuré (zipAes256Key manquant)"
    in if "fluxr" `isInfixOf` fp
       then useAes256
       else case dcSwitchDate cfg of
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

-- | Déchiffre un fichier @.zip@ sur disque en place (écrase avec le contenu déchiffré).
-- Le mode de déchiffrement est déterminé par 'modeForFile' avant l'appel.
decryptZipFile :: DecryptMode -- ^ Mode et clé(s) de déchiffrement.
               -> FilePath    -- ^ Chemin absolu vers le fichier à déchiffrer.
               -> IO (Either String ())
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

-- | Déchiffre tous les @.zip@ d'un répertoire local (récursif).
-- Après déchiffrement réussi, décompresse via @unzip@ et supprime le @.zip@ chiffré.
decryptDir :: DecryptConfig -- ^ Paramètres cryptographiques.
           -> FilePath      -- ^ Répertoire racine à parcourir.
           -> IO ()
decryptDir cfg dir = do
    createDirectoryIfMissing True dir
    putStrLn $ "Répertoire : " <> dir
    entries <- listDirectory dir
    mapM_ (processEntry cfg dir) entries

processEntry :: DecryptConfig -> FilePath -> FilePath -> IO ()
processEntry cfg dir name = do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
        then decryptDir cfg path
        else if ".zip" `isSuffixOf` name
               then do
                 putStr $ "  Déchiffrement : " <> name <> " ... "
                 case modeForFile cfg path of
                   Left err -> putStrLn $ "ERREUR (config) : " <> err
                   Right mode -> do
                     res <- decryptZipFile mode path
                     case res of
                       Left err -> putStrLn $ "ERREUR : " <> err
                       Right () -> do
                         let outDir  = takeDirectory path
                             tmpPath = path <> ".decrypting"
                         renameFile path tmpPath  -- évite la collision si le ZIP interne a le même nom
                         putStrLn $ "  → extraction vers : " <> outDir
                         beforeUnzip <- listDirectory outDir
                         (code, out, err) <- readProcessWithExitCode "unzip" ["-o", tmpPath, "-d", outDir] ""
                         case code of
                           ExitSuccess -> do
                             mapM_ (\l -> putStrLn $ "    " <> l) (filter (not . null) (lines out))
                             removeFile tmpPath
                             -- Diff avant/après : seulement les fichiers créés par CET unzip
                             afterUnzip <- listDirectory outDir
                             let innerZips = filter (".zip" `isSuffixOf`)
                                                    (filter (`notElem` beforeUnzip) afterUnzip)
                             forM_ innerZips $ \innerName -> do
                               let innerPath = outDir </> innerName
                                   innerTmp  = innerPath <> ".inner"
                               renameFile innerPath innerTmp
                               rawInner <- BS.readFile innerTmp
                               if BS.take 2 rawInner == BS.pack [0x50, 0x4B]
                                 then do
                                   putStrLn $ "  → extraction ZIP interne (non chiffré) : " <> innerName
                                   (code2, out2, err2) <- readProcessWithExitCode "unzip" ["-o", innerTmp, "-d", outDir] ""
                                   case code2 of
                                     ExitSuccess -> do
                                       mapM_ (\l -> putStrLn $ "      " <> l) (filter (not . null) (lines out2))
                                       removeFile innerTmp
                                     ExitFailure n2 -> do
                                       renameFile innerTmp innerPath
                                       putStrLn $ "ERREUR extraction ZIP interne (code " <> show n2 <> "): " <> err2
                                 else do
                                   putStrLn $ "  → déchiffrement + extraction ZIP interne : " <> innerName
                                   case modeForFile cfg innerPath of
                                     Left cfgErr -> do
                                       renameFile innerTmp innerPath
                                       putStrLn $ "ERREUR (config inner) : " <> cfgErr
                                     Right innerMode -> do
                                       decRes <- decryptZipFile innerMode innerTmp
                                       case decRes of
                                         Left decErr -> do
                                           renameFile innerTmp innerPath
                                           putStrLn $ "ERREUR déchiffrement ZIP interne : " <> decErr
                                         Right () -> do
                                           (code2, out2, err2) <- readProcessWithExitCode "unzip" ["-o", innerTmp, "-d", outDir] ""
                                           case code2 of
                                             ExitSuccess -> do
                                               mapM_ (\l -> putStrLn $ "      " <> l) (filter (not . null) (lines out2))
                                               removeFile innerTmp
                                             ExitFailure n2 -> do
                                               renameFile innerTmp innerPath
                                               putStrLn $ "ERREUR extraction ZIP interne (code " <> show n2 <> "): " <> err2
                             -- Niveau-3 : ZIPs plain réapparus avec le même nom qu'un inner traité
                             -- (ex. C68 : inner chiffré → déchiffré → ZIP plain du même nom)
                             afterInner <- listDirectory outDir
                             let level3Zips = filter (".zip" `isSuffixOf`)
                                                     (filter (`elem` innerZips) afterInner)
                             forM_ level3Zips $ \l3Name -> do
                               let l3Path = outDir </> l3Name
                                   l3Tmp  = l3Path <> ".inner"
                               rawL3 <- BS.readFile l3Path
                               when (BS.take 2 rawL3 == BS.pack [0x50, 0x4B]) $ do
                                 putStrLn $ "  → extraction ZIP niveau-3 (non chiffré) : " <> l3Name
                                 renameFile l3Path l3Tmp
                                 (code3, out3, err3) <- readProcessWithExitCode "unzip" ["-o", l3Tmp, "-d", outDir] ""
                                 case code3 of
                                   ExitSuccess -> do
                                     mapM_ (\l -> putStrLn $ "      " <> l) (filter (not . null) (lines out3))
                                     removeFile l3Tmp
                                   ExitFailure n3 -> do
                                     renameFile l3Tmp l3Path
                                     putStrLn $ "ERREUR extraction ZIP niveau-3 (code " <> show n3 <> "): " <> err3
                             putStrLn "  → OK"
                           ExitFailure n -> do
                             renameFile tmpPath path
                             putStrLn $ "ERREUR unzip (code " <> show n <> ")\n  stdout=" <> out <> "\n  stderr=" <> err
               else when ("_CR_" `isInfixOf` name && ".json" `isSuffixOf` name) $ do
                 raw <- BS.readFile path
                 if BS.length raw `mod` 16 /= 0
                   then putStrLn $ "  CR JSON en clair : " <> name <> " (ignoré)"
                   else do
                     putStr $ "  Déchiffrement CR : " <> name <> " ... "
                     case modeForFile cfg path of
                       Left err -> putStrLn $ "ERREUR (config) : " <> err
                       Right mode -> do
                         res <- decryptZipFile mode path
                         putStrLn $ case res of
                           Left err -> "ERREUR : " <> err
                           Right () -> "OK"
