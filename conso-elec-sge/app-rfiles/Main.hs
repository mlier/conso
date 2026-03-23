{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Control.Exception      (try, SomeException)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString     as BS
import           Data.ByteArray.Encoding (convertFromBase, Base(..))
import Conso.Fr.Elec.Sge.Rfiles.LoadRFiles
import Conso.Fr.Elec.Sge.Rfiles.DecryptRFiles


-- ---------------------------------------------------------------------------
-- Types de commande
-- ---------------------------------------------------------------------------

data Command
    = Ls
    | List ListOpts
    | Load LoadOpts
    | Decrypt DecryptOpts

data ListOpts = ListOpts
    { listVerbose :: Bool
    , listJours   :: DayLimit
    }

data LoadOpts = LoadOpts
    { loadDir     :: Maybe FilePath
    , loadArchive :: Bool
    , loadRemove  :: Bool
    , loadJours   :: DayLimit
    }

newtype DecryptOpts = DecryptOpts
    { decryptDir' :: Maybe FilePath }


-- ---------------------------------------------------------------------------
-- Parsers optparse-applicative
-- ---------------------------------------------------------------------------

joursReader :: ReadM DayLimit
joursReader = eitherReader $ \s ->
    if s == "tous"
        then Right AllDays
        else case reads s of
            [(n, "")] | n > 0 -> Right (Days n)
            _                  -> Left "Valeur invalide : entier > 0 ou \"tous\""

listParser :: Parser ListOpts
listParser = ListOpts
    <$> switch (long "verbose" <> short 'v'
                <> help "Afficher taille et date de modification")
    <*> option joursReader
            ( long "jours" <> short 'j' <> metavar "N|tous"
           <> value (Days 1)
           <> showDefaultWith (\case Days n -> show n; AllDays -> "tous")
           <> help "Ne lister que les fichiers de moins de N jours (tous = tous les fichiers)" )

loadParser :: Parser LoadOpts
loadParser = LoadOpts
    <$> optional (strOption (long "dir" <> short 'd' <> metavar "DIR"
                             <> help "Répertoire local de destination (remplace la config)"))
    <*> switch (long "archive"
                <> help "Déplacer les fichiers vers archiveDir après téléchargement")
    <*> switch (long "remove"
                <> help "Supprimer les fichiers du serveur après téléchargement (vérifie la taille)")
    <*> option joursReader
            ( long "jours" <> short 'j' <> metavar "N|tous"
           <> value (Days 1)
           <> showDefaultWith (\case Days n -> show n; AllDays -> "tous")
           <> help "Ne charger que les fichiers de moins de N jours (tous = tous les fichiers)" )

decryptParser :: Parser DecryptOpts
decryptParser = DecryptOpts
    <$> optional (strOption (long "dir" <> short 'd' <> metavar "DIR"
                             <> help "Répertoire à déchiffrer (défaut : localDir de la config)"))

commandParser :: Parser Command
commandParser = subparser
    (  command "ls"
        ( info (pure Ls <**> helper)
               ( progDesc "Lister le répertoire racine du serveur SFTP" ))
    <> command "list"
        ( info (List <$> listParser <**> helper)
               ( progDesc "Lister les fichiers .zip disponibles (sans modification)" ))
    <> command "load"
        ( info (Load <$> loadParser <**> helper)
               ( progDesc "Télécharger les fichiers .zip (--archive ou --remove pour post-traitement)" ))
    <> command "decrypt"
        ( info (Decrypt <$> decryptParser <**> helper)
               ( progDesc "Déchiffrer les fichiers .zip locaux (AES-256-CBC)" ))
    )


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Décode une clé hex depuis la config ; renvoie Left si invalide
hexToBytes :: String -> String -> Either String BS.ByteString
hexToBytes label hex =
    case convertFromBase Base16 (BS.pack (map (fromIntegral . fromEnum) hex)) of
        Left err -> Left $ label <> " invalide (hex attendu) : " <> err
        Right bs -> Right bs


-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    cmd <- execParser
        ( info (commandParser <**> helper)
               ( fullDesc
              <> progDesc "Gestion des fichiers R Enedis via SFTP"
              <> header   "manage-rfiles - chargement des fichiers R depuis le serveur Enedis" ))

    cfg <- getConfig

    result <- try $ case cmd of

        Ls -> lsRFiles cfg

        List o ->
            listRFiles cfg (listVerbose o) (listJours o) >>= \infos ->
                putStrLn $ "\n" <> show (length infos) <> " fichier(s) disponible(s)."

        Load o -> do
            let cfg'   = maybe cfg (\d -> cfg { localDir = d }) (loadDir o)
                postDl = case (loadArchive o, loadRemove o) of
                             (True, _) -> Archive
                             (_, True) -> Remove
                             _         -> Keep
            files <- loadRFiles cfg' postDl (loadJours o)
            putStrLn $ "\n" <> show (length files) <> " fichier(s) téléchargé(s)."

        Decrypt o -> do
            let dir = fromMaybe (localDir cfg) (decryptDir' o)
            let e128k = maybe (Right Nothing) (fmap Just . hexToBytes "zipAes128Key") (zipAes128Key cfg)
                e128v = maybe (Right Nothing) (fmap Just . hexToBytes "zipAes128IV")  (zipAes128IV  cfg)
                e256k = maybe (Right Nothing) (fmap Just . hexToBytes "zipAes256Key") (zipAes256Key cfg)
            case (e128k, e128v, e256k) of
                (Left err, _, _) -> putStrLn $ "Erreur : " <> err
                (_, Left err, _) -> putStrLn $ "Erreur : " <> err
                (_, _, Left err) -> putStrLn $ "Erreur : " <> err
                (Right k128, Right iv128, Right k256) -> do
                    let dc = DecryptConfig k128 iv128 k256 (zipAesSwitchDate cfg)
                    decryptDir dc dir
                    putStrLn "Déchiffrement terminé."

    case (result :: Either SomeException ()) of
        Left e  -> putStrLn $ "Erreur SFTP : " <> show e
        Right _ -> return ()
