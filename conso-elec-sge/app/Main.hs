{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Text.Pretty.Simple (pPrint)
import           System.Posix.User ()
import           GHC.Generics ()

import           Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10 as CDTC
import           Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type ( ConsulterDonneesTechniquesContractuellesResponseType )
import           Conso.Fr.Elec.Sge.ConsulterMesuresV11 as CM
import           Conso.Fr.Elec.Sge.ConsulterMesuresV11Type ( ConsulterMesuresResponseType )
import           Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3 as CMD
import           Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
    ( MesuresTypeCodeType(..)
    , MesuresPasType(..)
    , SensMesureType(..)
    , CadreAccesType(..)
    , ConsulterMesuresDetailleesV3ResponseType )

import           Conso.Fr.Elec.Sge.Sge (prettyXml)
import           Display (renderApp)
import           Display.InfoDisplay          ()   -- instances Renderable
import           Display.MesuresDisplay       ()   -- instances Renderable
import           Display.MesuresDetailDisplay ()   -- instances Renderable


data Options = Options
    {
    -- global options
      optVerbose     :: Bool
    , optXml         :: Bool
    , optRaw         :: Bool
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

data MesuresDetailOptions = MesuresDetailOptions
  { mdPoint        :: String
  , mdType         :: String
  , mdGrandeur     :: String
  , mdDebut        :: String
  , mdFin          :: String
  , mdPas          :: Maybe String
  , mdCorrigees    :: Bool
  , mdSens         :: String
  , mdAutorisation :: String
  } deriving (Eq, Show)


opts :: Parser Options
opts =
    Options
        <$> switch ( long "verbose" <> short 'v' <> help "Enable verbosity (default: disabled)" )
        <*> switch ( long "xml" <> help "Affiche la réponse XML brute du webservice" )
        <*> switch ( long "raw" <> help "Affiche la réponse brute non mise en forme (pPrint)" )
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
      <$> strOption   ( long "point"    <> short 'p' <> metavar "PRM"
                     <> help "Identifiant PRM du point" )
      <*> strOption   ( long "type"     <> short 't' <> metavar "COURBE|PMAX|ENERGIE|INDEX"
                     <> help "Type de mesure" )
      <*> strOption   ( long "grandeur" <> short 'g' <> metavar "PA|EA|TOUT|..."
                     <> help "Grandeur physique demandée" )
      <*> strOption   ( long "debut"    <> metavar "YYYY-MM-DD"
                     <> help "Date de début (incluse)" )
      <*> strOption   ( long "fin"      <> metavar "YYYY-MM-DD"
                     <> help "Date de fin (exclue)" )
      <*> optional (strOption ( long "pas" <> metavar "P1D|P1M"
                             <> help "Pas temporel (PMAX seulement)" ))
      <*> switch      ( long "corrigees"
                     <> help "Mesures corrigées BEST" )
      <*> strOption   ( long "sens"     <> metavar "INJECTION|SOUTIRAGE"
                     <> value "SOUTIRAGE" <> showDefault
                     <> help "Sens de la mesure" )
      <*> strOption   ( long "autorisation"
                     <> metavar "ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE"
                     <> value "ACCORD_CLIENT" <> showDefault
                     <> help "Cadre d'accès aux données" )

toTypeCode :: String -> MesuresTypeCodeType
toTypeCode "COURBE"  = MesuresTypeCodeTypeCOURBE
toTypeCode "PMAX"    = MesuresTypeCodeTypePMAX
toTypeCode "ENERGIE" = MesuresTypeCodeTypeENERGIE
toTypeCode "INDEX"   = MesuresTypeCodeTypeINDEX
toTypeCode s         = errorWithoutStackTrace $ "Type de mesure inconnu: " ++ s ++ " (COURBE|PMAX|ENERGIE|INDEX)"

toPas :: String -> MesuresPasType
toPas "P1D" = MesuresPasType_P1D
toPas "P1M" = MesuresPasType_P1M
toPas s     = errorWithoutStackTrace $ "Pas inconnu: " ++ s ++ " (P1D|P1M)"

toSens :: String -> SensMesureType
toSens "INJECTION" = SensMesureTypeINJECTION
toSens "SOUTIRAGE" = SensMesureTypeSOUTIRAGE
toSens s           = errorWithoutStackTrace $ "Sens inconnu: " ++ s ++ " (INJECTION|SOUTIRAGE)"

toAutorisation :: String -> CadreAccesType
toAutorisation "ACCORD_CLIENT" = CadreAccesTypeACCORDCLIENT
toAutorisation "SERVICE_ACCES" = CadreAccesTypeSERVICEACCES
toAutorisation "EST_TITULAIRE" = CadreAccesTypeESTTITULAIRE
toAutorisation s               = errorWithoutStackTrace $ "Autorisation inconnue: " ++ s ++ " (ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE)"


docommand :: Options -> IO ()
docommand Options{ optXml=xml, optRaw=raw, optCommand=c } = case c of
    Info i -> do
        myType <- CDTC.initType (pointIdInfo i) (autorisationClient i)
        if xml
          then CDTC.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- CDTC.wsRequest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
            if raw then pPrint rep else renderApp rep

    Mesures m -> do
        myType <- CM.initType (pointIdMesures m) True
        if xml
          then CM.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- CM.wsRequest myType :: IO (Either (String, String) ConsulterMesuresResponseType)
            if raw then pPrint rep else renderApp rep

    MesuresDetail m -> do
        myType <- CMD.initType
                    (mdPoint m)
                    (toTypeCode (mdType m))
                    (mdGrandeur m)
                    (mdDebut m)
                    (mdFin m)
                    (toPas <$> mdPas m)
                    (mdCorrigees m)
                    (toSens (mdSens m))
                    (toAutorisation (mdAutorisation m))
        if xml
          then CMD.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- CMD.wsRequest myType :: IO (Either (String, String) ConsulterMesuresDetailleesV3ResponseType)
            if raw then pPrint rep else renderApp rep


main :: IO ()
main = docommand =<< execParser optsHeader
  where
    optsHeader = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Consultation des webservices SGE Enedis"
     <> header "conso-elec-sge" )
