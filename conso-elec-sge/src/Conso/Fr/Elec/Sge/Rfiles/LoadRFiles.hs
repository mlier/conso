{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-|
Module      : Conso.Fr.Elec.Sge.Rfiles.LoadRFiles
Description : Téléchargement SFTP des fichiers Rxx depuis le serveur Enedis

Fournit les fonctions pour lister et télécharger les fichiers Rxx
déposés par Enedis sur un serveur SFTP.

La configuration est lue depuis @~\/.conso\/conso-elec-sge-env.yaml@, nœud @rfiles:@,
qui contient les identifiants SFTP, la clé AES-128 et la clé AES-256 (encodées en hexadécimal).

Authentification supportée :

  * Clé SSH — renseigner @keyFile@ (et @passphrase@ si la clé en a une)
  * Mot de passe — renseigner @password@, laisser @keyFile@ à @Nothing@
  * Agent SSH — laisser @keyFile@ et @password@ à @Nothing@

Usage :

> cfg   <- getConfig
> files <- listRFiles cfg True (Days 7)
> loadRFiles cfg Keep (Days 7)
-}
module Conso.Fr.Elec.Sge.Rfiles.LoadRFiles
  ( RFilesConfig(..)
  , RFileInfo(..)
  , PostDownload(..)
  , DayLimit(..)
  , getConfig
  , listRFiles
  , loadRFiles
  , lsRFiles
  ) where

import           GHC.Generics
import           Data.Yaml              (decodeFileEither)
import           Data.Aeson             (FromJSON(..), withObject, (.:), (.:?))
import qualified Data.ByteString.Char8  as BS
import           Data.Bits              ((.&.))
import           Data.List              (isSuffixOf)
import           Data.Maybe             (fromMaybe)
import           Control.Monad          (forM)
import           System.FilePath        ((</>), takeFileName, makeRelative, takeDirectory, isAbsolute)
import           System.Directory       (createDirectoryIfMissing, getFileSize)
import           System.Posix.User      (homeDirectory, getEffectiveUserName, getUserEntryForName)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime, getPOSIXTime)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           Network.SSH.Client.LibSSH2
import           Network.SSH.Client.LibSSH2.Foreign (saFileSize, saMtime, saPermissions)


-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Configuration SFTP et cryptographique lue depuis @~\/.conso\/conso-elec-sge-env.yaml@, nœud @rfiles:@.
data RFilesConfig = RFilesConfig
    { server     :: String
    -- ^ Nom d'hôte ou adresse IP du serveur SFTP.
    , port       :: Int
    -- ^ Port SFTP (généralement 22).
    , login      :: String
    -- ^ Nom d'utilisateur SFTP.
    , keyFile    :: Maybe FilePath
    -- ^ Chemin vers la clé privée SSH (@Nothing@ = authentification par mot de passe ou agent).
    , passphrase :: String
    -- ^ Passphrase de la clé SSH (chaîne vide si aucune).
    , password   :: Maybe String
    -- ^ Mot de passe SFTP (@Nothing@ si authentification par clé ou agent).
    , knownHosts :: FilePath
    -- ^ Chemin vers le fichier @known_hosts@, ex. : @\/home\/user\/.ssh\/known_hosts@.
    , remoteDir  :: Maybe FilePath
    -- ^ Répertoire source sur le serveur (@Nothing@ = racine @"."@).
    , archiveDir :: FilePath
    -- ^ Répertoire d'archive sur le serveur (utilisé par 'loadRFiles' avec 'Archive').
    , localDir   :: FilePath
    -- ^ Répertoire local de destination pour les fichiers téléchargés.
    , zipAes128Key     :: Maybe String
    -- ^ Clé AES-128 encodée en hexadécimal (32 caractères = 16 octets).
    , zipAes128IV      :: Maybe String
    -- ^ IV statique AES-128 encodé en hexadécimal (32 caractères = 16 octets).
    , zipAes256Key     :: Maybe String
    -- ^ Clé AES-256 encodée en hexadécimal (64 caractères = 32 octets).
    , zipAesSwitchDate :: Maybe String
    -- ^ Date de bascule AES-128 → AES-256, format @YYYYMMDD@ (@Nothing@ = tout AES-128).
    } deriving (Show, Generic)

-- | Instance manuelle : lit la config depuis les nœuds @server@, @local@ et @decrypt@.
instance FromJSON RFilesConfig where
    parseJSON = withObject "rfiles" $ \rfObj -> do
        srv <- rfObj .: "server"  >>= withObject "server"  pure
        loc <- rfObj .: "local"   >>= withObject "local"   pure
        dec <- rfObj .: "decrypt" >>= withObject "decrypt" pure
        RFilesConfig
            <$> srv .:  "server"
            <*> srv .:  "port"
            <*> srv .:  "login"
            <*> srv .:? "keyFile"
            <*> srv .:  "passphrase"
            <*> srv .:? "password"
            <*> srv .:  "knownHosts"
            <*> srv .:? "remoteDir"
            <*> srv .:  "archiveDir"
            <*> loc .:  "dir"
            <*> dec .:? "zipAes128Key"
            <*> dec .:? "zipAes128IV"
            <*> dec .:? "zipAes256Key"
            <*> dec .:? "zipAesSwitchDate"

-- | Wrapper interne : lit @sge.rfiles@ depuis le fichier YAML fusionné.
newtype ConsoRFilesFile = ConsoRFilesFile { getRFilesConfig :: RFilesConfig }

instance FromJSON ConsoRFilesFile where
    parseJSON = withObject "top" $ \topObj -> do
        sgeVal <- topObj .: "sge"
        withObject "sge" (\sgeObj -> ConsoRFilesFile <$> sgeObj .: "rfiles") sgeVal


-- ---------------------------------------------------------------------------
-- Informations sur un fichier distant
-- ---------------------------------------------------------------------------

-- | Métadonnées d'un fichier présent sur le serveur SFTP.
data RFileInfo = RFileInfo
    { rfiRelPath :: FilePath
    -- ^ Chemin relatif à @remoteDir@, ex. : @"2024\/01\/file.zip"@.
    , rfiSize    :: Integer
    -- ^ Taille du fichier en octets.
    , rfiMtime   :: Integer
    -- ^ Date de dernière modification (timestamp Unix).
    } deriving (Show)


-- ---------------------------------------------------------------------------
-- Lecture de la config
-- ---------------------------------------------------------------------------

myHomeDirectory :: IO FilePath
myHomeDirectory = do
    name  <- getEffectiveUserName
    entry <- getUserEntryForName name
    return $ homeDirectory entry

-- | Lit la configuration depuis @~\/.conso\/conso-elec-sge-env.yaml@, nœud @rfiles:@.
-- Lève une exception si le fichier est absent ou mal formé.
getConfig :: IO RFilesConfig
getConfig = do
    home <- myHomeDirectory
    wrapper <- either (error . show) id <$>
        decodeFileEither (home </> ".conso" </> "conso-elec-sge-env.yaml")
    return (getRFilesConfig wrapper)


-- ---------------------------------------------------------------------------
-- Connexion SFTP
-- ---------------------------------------------------------------------------

withRFilesSFTP :: RFilesConfig -> (Sftp -> IO a) -> IO a
withRFilesSFTP cfg action =
    let kh = knownHosts cfg
        lo = login cfg
        h  = server cfg
        p  = port cfg
    in case keyFile cfg of
        Just kf ->
            withSFTP kh (kf <> ".pub") kf (passphrase cfg) lo h p action
        Nothing ->
            withSFTPUser kh lo (fromMaybe "" $ password cfg) h p action


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isDirectory :: SftpAttributes -> Bool
isDirectory attrs =
    (fromIntegral (saPermissions attrs) .&. (0o170000 :: Int)) == 0o040000

isZipFile :: RFileInfo -> Bool
isZipFile info = ".zip" `isSuffixOf` takeFileName (rfiRelPath info)

formatSize :: Integer -> String
formatSize n
    | n < 1024       = show n <> " B"
    | n < 1024*1024  = show (n `div` 1024) <> " KB"
    | otherwise      = show (n `div` (1024*1024)) <> " MB"

formatMtime :: Integer -> String
formatMtime t =
    formatTime defaultTimeLocale "%Y-%m-%d %H:%M" $
        posixSecondsToUTCTime (fromIntegral t)


-- ---------------------------------------------------------------------------
-- Résolution du répertoire de départ
-- ---------------------------------------------------------------------------

-- | Résout le répertoire de départ en chemin relatif SFTP.
-- "c5"   → "./c5"
-- "/c5"  → "/c5"  (inchangé)
-- "."    → "."
-- Nothing → "."
startDir :: RFilesConfig -> FilePath
startDir cfg = case remoteDir cfg of
    Nothing               -> "."
    Just p | isAbsolute p          -> p
           | p == "." || p == "./" -> "."
           | otherwise             -> "." </> p


-- ---------------------------------------------------------------------------
-- Parcours récursif du répertoire distant
-- ---------------------------------------------------------------------------

listTree :: Sftp -> FilePath -> FilePath -> IO [RFileInfo]
listTree sftp baseDir dir = do
    entries <- sftpListDir sftp dir
    fmap concat $ forM entries $ \(nameBS, attrs) -> do
        let name = takeFileName $ BS.unpack nameBS
        if name `elem` [".", ".."]
            then return []
            else do
                let fullPath = dir </> name
                    relPath  = makeRelative baseDir fullPath
                if isDirectory attrs
                    then listTree sftp baseDir fullPath
                    else return [RFileInfo relPath
                                    (fromIntegral (saFileSize attrs))
                                    (fromIntegral (saMtime attrs))]


-- ---------------------------------------------------------------------------
-- Commande ls : liste le répertoire racine SFTP (1 niveau, sans récursion)
-- ---------------------------------------------------------------------------

-- | Liste le contenu du répertoire racine SFTP (1 niveau, sans récursion).
-- Les entrées @.@ et @..@ sont masquées.
lsRFiles :: RFilesConfig -- ^ Configuration SFTP.
         -> IO ()
lsRFiles cfg = withRFilesSFTP cfg $ \sftp -> do
    entries <- sftpListDir sftp "."
    let visible = filter (\(n,_) -> BS.unpack n `notElem` [".",".."]) entries
    if null visible
        then putStrLn "(répertoire vide)"
        else mapM_ printEntry visible
  where
    printEntry (nameBS, attrs) = do
        let name = BS.unpack nameBS
        putStrLn $ if isDirectory attrs then name <> "/" else name


-- ---------------------------------------------------------------------------
-- Commande list : liste les fichiers sans y toucher
-- ---------------------------------------------------------------------------

-- | Liste les fichiers @.zip@ disponibles sur le serveur, sans les télécharger.
-- Retourne aussi la liste pour usage programmatique.
listRFiles :: RFilesConfig -- ^ Configuration SFTP.
           -> Bool         -- ^ @True@ = affiche taille et date de modification.
           -> DayLimit     -- ^ Filtre par ancienneté.
           -> IO [RFileInfo]
listRFiles cfg verbose dayLimit = withRFilesSFTP cfg $ \sftp -> do
    let root = startDir cfg
    now   <- round <$> getPOSIXTime
    infos <- filter (isRecentEnough dayLimit now) . filter isZipFile
                <$> listTree sftp root root
    if null infos
        then putStrLn "Aucun fichier .zip disponible."
        else mapM_ (printInfo verbose) infos
    return infos
  where
    printInfo False info = putStrLn (rfiRelPath info)
    printInfo True  info = putStrLn $
        rfiRelPath info
        <> "  " <> formatSize  (rfiSize  info)
        <> "  " <> formatMtime (rfiMtime info)


-- ---------------------------------------------------------------------------
-- Commande load : télécharge (et éventuellement archive ou supprime)
-- ---------------------------------------------------------------------------

-- | Action à effectuer sur le fichier distant après téléchargement réussi.
data PostDownload
    = Keep    -- ^ Conserver le fichier sur le serveur.
    | Archive -- ^ Déplacer le fichier dans 'archiveDir'.
    | Remove  -- ^ Supprimer le fichier du serveur (après vérification de taille).
    deriving (Show)

-- | Limite d'ancienneté pour filtrer les fichiers à télécharger.
data DayLimit
    = Days Int -- ^ Ne traiter que les fichiers modifiés dans les N derniers jours.
    | AllDays  -- ^ Traiter tous les fichiers sans limite d'ancienneté.

isRecentEnough :: DayLimit -> Integer -> RFileInfo -> Bool
isRecentEnough AllDays   _   _    = True
isRecentEnough (Days n) now info  = now - rfiMtime info <= fromIntegral n * 86400

-- | Télécharge les fichiers @.zip@ depuis le serveur SFTP dans 'localDir'.
-- Après chaque téléchargement, applique l'action 'PostDownload' sur le fichier distant.
-- Retourne la liste des chemins locaux créés.
loadRFiles :: RFilesConfig -- ^ Configuration SFTP.
           -> PostDownload -- ^ Action post-téléchargement.
           -> DayLimit     -- ^ Filtre par ancienneté.
           -> IO [FilePath]
loadRFiles cfg postDl dayLimit =
    withRFilesSFTP cfg $ \sftp -> do
        putStrLn $ "Début chargement, option " <> show postDl
        let root = startDir cfg
        now   <- round <$> getPOSIXTime
        infos <- filter (isRecentEnough dayLimit now) . filter isZipFile
                    <$> listTree sftp root root
        putStrLn $ "Nombre de document à télécharger : " <> show ( length infos )
        mapM (downloadFile sftp root) infos
  where
    downloadFile sftp root info = do
        let rel     = rfiRelPath info
            local'  = localDir cfg </> rel
            remote' = root </> rel
        createDirectoryIfMissing True (takeDirectory local')
        _ <- sftpReceiveFile sftp local' remote'
        case postDl of
            Keep    -> putStrLn $ "Téléchargé : " <> rel
            Archive -> do
                let archive' = archiveDir cfg </> rel
                sftpRenameFile sftp remote' archive'
                putStrLn $ "Téléchargé et archivé : " <> rel
            Remove  -> do
                localSize <- getFileSize local'
                if toInteger localSize == rfiSize info
                    then do
                        sftpDeleteFile sftp remote'
                        putStrLn $ "Téléchargé et supprimé : " <> rel
                    else
                        putStrLn $ "ATTENTION : taille incorrecte (" <>
                            show localSize <> " ≠ " <> show (rfiSize info) <>
                            "), fichier conservé sur le serveur : " <> rel
        return local'
