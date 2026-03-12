{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Conso.Fr.Elec.Rfiles.LoadRFiles
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
import           Data.Yaml              (FromJSON, decodeFileEither)
import qualified Data.ByteString.Char8  as BS
import           Data.Bits              ((.&.))
import           Data.List              (isSuffixOf)
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

-- | Configuration lue depuis ~/.conso/rfiles.yaml
--
-- Authentification :
--   - avec clé SSH  : renseigner keyFile (et passphrase si la clé en a une)
--   - avec mot de passe : renseigner password, laisser keyFile à null
--   - via agent SSH : laisser keyFile et password à null
data RFilesConfig = RFilesConfig
    { server     :: String
    , port       :: Int
    , login      :: String
    , keyFile    :: Maybe FilePath   -- clé privée SSH
    , passphrase :: String           -- passphrase de la clé (vide = aucune)
    , password   :: Maybe String     -- mot de passe (si pas de clé)
    , knownHosts :: FilePath         -- ex: /home/user/.ssh/known_hosts
    , remoteDir  :: Maybe FilePath    -- répertoire source sur le serveur (Nothing = racine "/")
    , archiveDir :: FilePath         -- répertoire d'archive sur le serveur
    , localDir   :: FilePath         -- répertoire local de destination
    } deriving (Show, Generic)

instance FromJSON RFilesConfig


-- ---------------------------------------------------------------------------
-- Informations sur un fichier distant
-- ---------------------------------------------------------------------------

data RFileInfo = RFileInfo
    { rfiRelPath :: FilePath   -- relatif à remoteDir, ex: "2024/01/file.zip"
    , rfiSize    :: Integer    -- octets
    , rfiMtime   :: Integer    -- timestamp Unix
    } deriving (Show)


-- ---------------------------------------------------------------------------
-- Lecture de la config
-- ---------------------------------------------------------------------------

myHomeDirectory :: IO FilePath
myHomeDirectory = do
    name  <- getEffectiveUserName
    entry <- getUserEntryForName name
    return $ homeDirectory entry

getConfig :: IO RFilesConfig
getConfig = do
    home <- myHomeDirectory
    either (error . show) id <$>
        decodeFileEither (home </> ".conso" </> "rfiles.yaml")


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
            withSFTPUser kh lo (maybe "" id $ password cfg) h p action


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

lsRFiles :: RFilesConfig -> IO ()
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

listRFiles :: RFilesConfig -> Bool -> DayLimit -> IO [RFileInfo]
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

data PostDownload = Keep | Archive | Remove
        deriving (Show)

data DayLimit = Days Int | AllDays

isRecentEnough :: DayLimit -> Integer -> RFileInfo -> Bool
isRecentEnough AllDays   _   _    = True
isRecentEnough (Days n) now info  = now - rfiMtime info <= fromIntegral n * 86400

loadRFiles :: RFilesConfig -> PostDownload -> DayLimit -> IO [FilePath]
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
