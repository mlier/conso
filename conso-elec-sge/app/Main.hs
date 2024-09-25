{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Data.Text ( Text )
import           System.Posix.User ()
import           GHC.Generics ()

import           Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10 as CDTC
import           Conso.Fr.Elec.Sge.ConsulterMesuresV11 as CM
import           Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3 as CMD

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

data InfoOptions = InfoOptions
  { pointIdInfo :: String
  , autorisationClient :: Bool
  } deriving (Eq, Show)

newtype MesuresOptions = MesuresOptions
  { pointIdMesures     :: String
  } deriving (Eq, Show)

newtype MesuresDetailOptions = MesuresDetailOptions
  { pointIdMesuresDetail     :: String
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
      <*> switch
          ( long "autorisation"
         <> short 'a'
         <> help "Enable verbosity (default: disabled)" )

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
        myType <- CDTC.initType (pointIdInfo i) (autorisationClient i)
        CDTC.wsRequest myType

    Mesures m -> do
        myType <- CM.initType (pointIdMesures m)
        CM.wsRequest myType

    MesuresDetail m -> do
        putStrLn "To be done : mesures detail"
        putStrLn $ pointIdMesuresDetail m
        --myType <- CMD.initType (pointIdMesuresDetail m)
        --     mesuresTypeCode grandeurPhysique dateDebut dateFin 
        -- mesuresPas mesuresCorrigees sens cadreAcces
        --CMD.wsRequest myType


main :: IO ()
main = docommand =<< execParser optsHeader
  where
    optsHeader = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
