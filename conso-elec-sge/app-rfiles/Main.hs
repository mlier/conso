{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Control.Exception      (try, SomeException)
import Conso.Fr.Elec.Rfiles.LoadRFiles


-- ---------------------------------------------------------------------------
-- Types de commande
-- ---------------------------------------------------------------------------

data Command
    = Ls
    | List ListOpts
    | Load LoadOpts

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
    )


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

    case (result :: Either SomeException ()) of
        Left e  -> putStrLn $ "Erreur SFTP : " <> show e
        Right _ -> return ()
