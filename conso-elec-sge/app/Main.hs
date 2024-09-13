{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Data.Text ( Text ) 
import           System.Posix.User ()
import           GHC.Generics ()

import           ConsulterDonneesTechniquesContractuellesV10

data Options = Options
    {
    -- global options
      optVerbose     :: Bool
    -- commands
    , optCommand     :: Command
    } deriving (Eq, Show)

data Command
    = Info InfoOptions
    | Mesures MesuresOptions
    | MesuresDetail MesuresDetailOptions
{-    | Recherche RechercheCommand
    | HistoriqueM23 HistoriqueM23Command
    | FluxCommand FluxCommand
    | FluxRecherche FluxRechercheCommand
    | FluxArret FluxArretCommand
    | FluxInfra FluxInfraCommand
-}  deriving (Eq, Show)

newtype InfoOptions = InfoOptions
  { pointInfo     :: String
  } deriving (Eq, Show)

newtype MesuresOptions = MesuresOptions
  { pointMesures     :: String
  } deriving (Eq, Show)

newtype MesuresDetailOptions = MesuresDetailOptions
  { pointMesuresDetail     :: String
  } deriving (Eq, Show)


opts :: Parser Options
opts =
    Options
        <$> switch ( long "verbose" <> short 'v' <> help "Enable verbosity (default: disabled)" )
        <*> comm

comm :: Parser Command
comm =
    subparser
        (  command "info"
            (   Info
            <$> info
                ( infParser <**> helper )
                ( progDesc "Obtient les infos" )
            )
        <> command "mesures"
            (info
                ( Mesures <$> mesuresParser <**> helper )
                ( progDesc "Avoir des mesures mensuelles" )
            )
        <> command "mesuresdetail"
            (info
                ( MesuresDetail <$> mesuresDetailParser <**> helper )
                ( progDesc "Avoir des mesures détaillées" )
            )
        )

infParser :: Parser InfoOptions
infParser = InfoOptions
      <$> strOption
          ( long "point"
         <> short 'p' 
         <> metavar "POINT"
         <> help "Point" )

mesuresParser :: Parser MesuresOptions
mesuresParser = MesuresOptions
      <$> strOption
          ( long "point"
         <> short 'p' 
         <> metavar "POINT"
         <> help "Point" )

mesuresDetailParser :: Parser MesuresDetailOptions
mesuresDetailParser = MesuresDetailOptions
      <$> strOption
          ( long "point"
         <> short 'p' 
         <> metavar "POINT"
         <> help "Point" )

docommand :: Options -> IO ()
docommand Options{ optVerbose=v, optCommand=c } = case c of 
    Info i -> do 
        putStrLn "To be done : info"
        putStrLn $ pointInfo i
        print v
        --getSge

    Mesures m -> do
        putStrLn "To be done : mesures"
        putStrLn $ pointMesures m
        print v
        let pointId = "21429667044956" :: Text
        print v
       
    MesuresDetail m -> do
        putStrLn "To be done : mesures detail"
        putStrLn $ pointMesuresDetail m
        print v
        let pointId = "21429667044956" :: Text
        print v
      

main :: IO ()
main = docommand =<< execParser optsHeader
  where
    optsHeader = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
