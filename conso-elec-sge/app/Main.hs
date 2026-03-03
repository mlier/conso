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

import           Conso.Fr.Elec.Sge.RechercherPointV20 as RP
import           Conso.Fr.Elec.Sge.RechercherPointV20Type (RechercherPointResponseType)
import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
    ( DomaineTensionCodeType(..), ClientFinalCategorieCodeType(..) )

import           Conso.Fr.Elec.Sge.Sge (prettyXml)
import           Display (renderApp)
import           Display.InfoDisplay          ()   -- instances Renderable
import           Display.MesuresDisplay       ()   -- instances Renderable
import           Display.MesuresDetailDisplay ()   -- instances Renderable
import           Display.RechercheDisplay     ()   -- instances Renderable


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
    | MesuresDetail MesuresDetailCommand
    | Recherche RechercheOptions
    deriving (Eq, Show)

data RechercheOptions = RechercheOptions
  { rEscalier   :: Maybe String
  , rBatiment   :: Maybe String
  , rVoie       :: Maybe String
  , rLieuDit    :: Maybe String
  , rCodePostal :: Maybe String
  , rCommune    :: Maybe String
  , rSiret      :: Maybe String
  , rMatricule  :: Maybe String
  , rDomaine    :: Maybe String
  , rNom        :: Maybe String
  , rCategorie  :: Maybe String
  , rHorsPerim  :: Maybe Bool
  } deriving (Eq, Show)

data InfoOptions = InfoOptions
  { pointIdInfo :: String
  , autorisationClient :: Bool
  } deriving (Eq, Show)

newtype MesuresOptions = MesuresOptions
  { pointIdMesures     :: String
  } deriving (Eq, Show)

-- | Options communes aux 4 sous-commandes de mesuresdetail.
data MdCommonOpts = MdCommonOpts
  { mdcPoint        :: String
  , mdcGrandeur     :: String
  , mdcDebut        :: String
  , mdcFin          :: String
  , mdcCorrigees    :: Bool
  , mdcSens         :: String
  , mdcAutorisation :: String
  } deriving (Eq, Show)

-- | Sous-commande choisie par l'utilisateur ; le constructeur détermine le type de mesure.
data MesuresDetailCommand
    = MdCourbe  MdCommonOpts
    | MdPmax    MdCommonOpts String   -- ^ 2e champ = pas (P1D|P1M), obligatoire pour PMAX
    | MdEnergie MdCommonOpts
    | MdIndex   MdCommonOpts
  deriving (Eq, Show)


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
            ( info
                ( MesuresDetail <$> mesuresDetailComm )
                ( progDesc "Avoir des mesures détaillées (courbe|pmax|energie|index)" )
            )
        <> command "recherche"
            ( info
                ( Recherche <$> rechercheParser <**> helper )
                ( progDesc "Rechercher des points par critères (adresse, nom, domaine…)" )
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

-- | Options communes aux 4 sous-commandes ; le metavar de --grandeur est spécifique à chaque type.
mdCommonParser :: String -> Parser MdCommonOpts
mdCommonParser grandeurMeta = MdCommonOpts
      <$> strOption ( long "point"    <> short 'p' <> metavar "PRM"
                   <> help "Identifiant PRM du point" )
      <*> strOption ( long "grandeur" <> short 'g' <> metavar grandeurMeta
                   <> help "Grandeur physique" )
      <*> strOption ( long "debut"    <> metavar "YYYY-MM-DD"
                   <> help "Date de début (incluse)" )
      <*> strOption ( long "fin"      <> metavar "YYYY-MM-DD"
                   <> help "Date de fin (exclue)" )
      <*> switch    ( long "corrigees"
                   <> help "Mesures corrigées BEST" )
      <*> strOption ( long "sens"     <> metavar "INJECTION|SOUTIRAGE"
                   <> value "SOUTIRAGE" <> showDefault
                   <> help "Sens de la mesure" )
      <*> strOption ( long "autorisation"
                   <> metavar "ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE"
                   <> value "ACCORD_CLIENT" <> showDefault
                   <> help "Cadre d'accès aux données" )

mesuresDetailComm :: Parser MesuresDetailCommand
mesuresDetailComm = subparser
    (  command "courbe"
        ( info ( MdCourbe <$> mdCommonParser "PA|PRI|PRC|E|TOUT" <**> helper )
               ( progDesc "Courbe de puissance ou tension" ) )
    <> command "pmax"
        ( info ( MdPmax
                   <$> mdCommonParser "PMA|TOUT"
                   <*> strOption ( long "pas" <> metavar "P1D|P1M"
                                <> help "Pas temporel (quotidien ou mensuel)" )
                   <**> helper )
               ( progDesc "Puissance maximale quotidienne ou mensuelle" ) )
    <> command "energie"
        ( info ( MdEnergie <$> mdCommonParser "EA|ERC|ERI" <**> helper )
               ( progDesc "Énergie globale quotidienne" ) )
    <> command "index"
        ( info ( MdIndex <$> mdCommonParser "EA|ER|ERC|ERI|DD|DE|DQ|PMA|TF|TOUT" <**> helper )
               ( progDesc "Index" ) )
    )


rechercheParser :: Parser RechercheOptions
rechercheParser = RechercheOptions
    <$> optional (strOption (long "escalier"    <> metavar "TEXTE"          <> help "Escalier/étage/appartement"))
    <*> optional (strOption (long "batiment"    <> metavar "TEXTE"          <> help "Bâtiment"))
    <*> optional (strOption (long "voie"        <> metavar "TEXTE"          <> help "Numéro et nom de voie"))
    <*> optional (strOption (long "lieu-dit"    <> metavar "TEXTE"          <> help "Lieu-dit"))
    <*> optional (strOption (long "code-postal" <> short 'c' <> metavar "CPPPP"  <> help "Code postal"))
    <*> optional (strOption (long "commune"     <> short 'i' <> metavar "XXXXX"  <> help "Code INSEE commune"))
    <*> optional (strOption (long "siret"       <> metavar "SIRET"          <> help "Numéro SIRET"))
    <*> optional (strOption (long "matricule"   <> metavar "TEXTE"          <> help "Matricule ou numéro de série"))
    <*> optional (strOption (long "domaine"     <> metavar "BTINF|BTSUP|HTA|HTB" <> help "Domaine de tension"))
    <*> optional (strOption (long "nom"         <> metavar "TEXTE"          <> help "Nom du client final"))
    <*> optional (strOption (long "categorie"   <> metavar "PRO|RES"        <> help "Catégorie client final"))
    <*> flag Nothing (Just True) (long "hors-perimetre" <> short 'r' <> help "Rechercher hors périmètre")


toDomaineTension :: String -> DomaineTensionCodeType
toDomaineTension "BTINF" = DomaineTensionCodeTypeBTINF
toDomaineTension "BTSUP" = DomaineTensionCodeTypeBTSUP
toDomaineTension "HTA"   = DomaineTensionCodeTypeHTA
toDomaineTension "HTB"   = DomaineTensionCodeTypeHTB
toDomaineTension s       = errorWithoutStackTrace $ "Domaine inconnu: " ++ s ++ " (BTINF|BTSUP|HTA|HTB)"


toCategorieClient :: String -> ClientFinalCategorieCodeType
toCategorieClient "PRO" = ClientFinalCategorieCodeTypePRO
toCategorieClient "RES" = ClientFinalCategorieCodeTypeRES
toCategorieClient s     = errorWithoutStackTrace $ "Catégorie inconnue: " ++ s ++ " (PRO|RES)"


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

    MesuresDetail cmd -> do
        let (common, typeCode, maybePas) = case cmd of
                MdCourbe  o     -> (o, MesuresTypeCodeTypeCOURBE,  Nothing)
                MdPmax    o pas -> (o, MesuresTypeCodeTypePMAX,    Just (toPas pas))
                MdEnergie o     -> (o, MesuresTypeCodeTypeENERGIE, Nothing)
                MdIndex   o     -> (o, MesuresTypeCodeTypeINDEX,   Nothing)
        myType <- CMD.initType
                    (mdcPoint common)
                    typeCode
                    (mdcGrandeur common)
                    (mdcDebut common)
                    (mdcFin common)
                    maybePas
                    (mdcCorrigees common)
                    (toSens (mdcSens common))
                    (toAutorisation (mdcAutorisation common))
        if xml
          then CMD.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- CMD.wsRequest myType :: IO (Either (String, String) ConsulterMesuresDetailleesV3ResponseType)
            if raw then pPrint rep else renderApp rep

    Recherche r -> do
        myType <- RP.initType
                    (rEscalier r)
                    (rBatiment r)
                    (rVoie r)
                    (rLieuDit r)
                    (rCodePostal r)
                    (rCommune r)
                    (rSiret r)
                    (rMatricule r)
                    (toDomaineTension <$> rDomaine r)
                    (rNom r)
                    (toCategorieClient <$> rCategorie r)
                    (rHorsPerim r)
        if xml
          then RP.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- RP.wsRequest myType :: IO (Either (String, String) RechercherPointResponseType)
            if raw then pPrint rep else renderApp rep


main :: IO ()
main = docommand =<< execParser optsHeader
  where
    optsHeader = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Consultation des webservices SGE Enedis"
     <> header "conso-elec-sge" )
