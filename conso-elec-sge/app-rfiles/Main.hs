{- HLINT ignore "Use newtype instead of data" -}
module Main where

import Options.Applicative
import Control.Exception      (try, SomeException)
import Conso.Fr.Elec.Rfiles.LoadRFiles


-- ---------------------------------------------------------------------------
-- Types de commande
-- ---------------------------------------------------------------------------

data Command
    = Read ReadOpts
    | Load LoadOpts

data ReadOpts = ReadOpts
    { readVerbose :: Bool }

data LoadOpts = LoadOpts
    { loadDir :: Maybe FilePath }


-- ---------------------------------------------------------------------------
-- Parsers optparse-applicative
-- ---------------------------------------------------------------------------

readParser :: Parser ReadOpts
readParser = ReadOpts
    <$> switch (long "verbose" <> short 'v'
                <> help "Afficher taille et date de modification")

loadParser :: Parser LoadOpts
loadParser = LoadOpts
    <$> optional (strOption (long "dir" <> short 'd' <> metavar "DIR"
                             <> help "Répertoire local de destination (remplace la config)"))

commandParser :: Parser Command
commandParser = subparser
    (  command "read"
        ( info (Read <$> readParser <**> helper)
               ( progDesc "Lister les fichiers .zip disponibles sur le serveur (sans modification)" ))
    <> command "load"
        ( info (Load <$> loadParser <**> helper)
               ( progDesc "Télécharger les fichiers .zip et les déplacer en archive sur le serveur" ))
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

        Read o ->
            readRFiles cfg (readVerbose o) >>= \infos ->
                putStrLn $ "\n" <> show (length infos) <> " fichier(s) disponible(s)."

        Load o -> do
            let cfg' = maybe cfg (\d -> cfg { localDir = d }) (loadDir o)
            files <- loadRFiles cfg'
            putStrLn $ "\n" <> show (length files) <> " fichier(s) téléchargé(s)."

    case (result :: Either SomeException ()) of
        Left e  -> putStrLn $ "Erreur SFTP : " <> show e
        Right _ -> return ()
