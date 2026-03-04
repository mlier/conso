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
import           Display.M023Display          (AffaireIdResult(..))  -- instance Renderable

import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10              as MFI
import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10        as MFA
import qualified Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10 as ITC
import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type              as MFI_T
import qualified Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10Type       as MFA_T
import qualified Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type as ITC_T
import           Text.XML.HaXml.Schema.Schema (SimpleType(simpleTypeText))

import qualified Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10      as ACCES
import           Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type  (CommanderAccesDonneesMesuresResponseType)
import           Display.AccesDisplay                                    ()   -- instance Renderable


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
    | M023 M023Command
    | Acces AccesOptions
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

data M023Command
    = M023Fines       MFIOptions
    | M023Facturantes MFAOptions
    | M023ITC         ITCOptions
    deriving (Eq, Show)

data MFIOptions = MFIOptions
  { mfiPoints    :: [String]
  , mfiType      :: String
  , mfiDebut     :: String
  , mfiFin       :: String
  , mfiCorrigees :: Maybe Bool
  , mfiSens      :: String
  , mfiCadre     :: String
  } deriving (Eq, Show)

data MFAOptions = MFAOptions
  { mfaPoints :: [String]
  , mfaDebut  :: String
  , mfaFin    :: String
  , mfaSens   :: String
  , mfaCadre  :: String
  } deriving (Eq, Show)

data ITCOptions = ITCOptions
  { itcPoints :: [String]
  , itcSens   :: String
  , itcCadre  :: String
  } deriving (Eq, Show)

data AccesAccordOpts
    = AccesPhysique String
    | AccesMorale   String
    deriving (Eq, Show)

data AccesOptions = AccesOptions
  { accesPoint  :: String
  , acesDuree   :: Maybe Integer
  , accesType   :: String
  , accesSens   :: String
  , accesAccord :: AccesAccordOpts
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
            ( info
                ( MesuresDetail <$> mesuresDetailComm )
                ( progDesc "Avoir des mesures détaillées (courbe|pmax|energie|index)" )
            )
        <> command "recherche"
            ( info
                ( Recherche <$> rechercheParser <**> helper )
                ( progDesc "Rechercher des points par critères (adresse, nom, domaine…)" )
            )
        <> command "m023"
            ( info
                ( M023 <$> m023Parser )
                ( progDesc "Demander publication de données M023 (fines|facturantes|itc)" )
            )
        <> command "acces"
            ( info
                ( Acces <$> accesParser <**> helper )
                ( progDesc "Commander accès aux données de mesures (AME)" )
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


m023Parser :: Parser M023Command
m023Parser = subparser
    (  command "fines"
        ( info ( M023Fines <$> mfiParser <**> helper )
               ( progDesc "Mesures fines R63–R66 (courbes, index, énergie, Pmax)" ) )
    <> command "facturantes"
        ( info ( M023Facturantes <$> mfaParser <**> helper )
               ( progDesc "Mesures facturantes R67" ) )
    <> command "itc"
        ( info ( M023ITC <$> itcParser <**> helper )
               ( progDesc "Infos techniques et contractuelles C68" ) )
    )

mfiParser :: Parser MFIOptions
mfiParser = MFIOptions
    <$> some      (strOption (long "point" <> short 'p' <> metavar "PRM"
                             <> help "Identifiant PRM (répétable)"))
    <*> strOption  (long "type"  <> short 't' <> metavar "COURBES|ENERGIE|PMAX|INDEX"
                   <> help "Type de mesures demandé")
    <*> strOption  (long "debut" <> metavar "YYYY-MM-DD" <> help "Date de début (incluse)")
    <*> strOption  (long "fin"   <> metavar "YYYY-MM-DD" <> help "Date de fin (exclue)")
    <*> optional   (   flag' True  (long "corrigees" <> help "Mesures corrigées (COURBES C1-C4/P1-P3)")
                   <|> flag' False (long "brutes"    <> help "Mesures brutes (COURBES)"))
    <*> strOption  (long "sens"  <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> strOption  (long "cadre" <> metavar "ACCORD_CLIENT|SERVICE_ACCES"
                   <> value "ACCORD_CLIENT" <> showDefault <> help "Cadre d'accès")

mfaParser :: Parser MFAOptions
mfaParser = MFAOptions
    <$> some      (strOption (long "point" <> short 'p' <> metavar "PRM"
                             <> help "Identifiant PRM (répétable)"))
    <*> strOption  (long "debut" <> metavar "YYYY-MM-DD" <> help "Date de début (incluse)")
    <*> strOption  (long "fin"   <> metavar "YYYY-MM-DD" <> help "Date de fin (exclue)")
    <*> strOption  (long "sens"  <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> strOption  (long "cadre" <> metavar "ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE"
                   <> value "ACCORD_CLIENT" <> showDefault <> help "Cadre d'accès")

itcParser :: Parser ITCOptions
itcParser = ITCOptions
    <$> some      (strOption (long "point" <> short 'p' <> metavar "PRM"
                             <> help "Identifiant PRM (répétable)"))
    <*> strOption  (long "sens"  <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> strOption  (long "cadre" <> metavar "ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE"
                   <> value "ACCORD_CLIENT" <> showDefault <> help "Cadre d'accès")


accesParser :: Parser AccesOptions
accesParser = AccesOptions
    <$> strOption  (long "point"  <> short 'p' <> metavar "PRM"
                   <> help "Identifiant PRM du point")
    <*> optional   (option auto (long "duree"  <> metavar "JOURS"
                   <> help "Durée de l'accès en jours (max 3×364 pour C5/P4)"))
    <*> strOption  (long "type"   <> short 't' <> metavar "CDC|IDX|ENERGIE|PMAX"
                   <> help "Type de données demandé")
    <*> strOption  (long "sens"   <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> accesAccordParser

accesAccordParser :: Parser AccesAccordOpts
accesAccordParser =
    (AccesPhysique <$> strOption (long "nom"          <> metavar "NOM"
                   <> help "Nom de la personne physique ayant donné accord"))
    <|>
    (AccesMorale   <$> strOption (long "denomination" <> metavar "DENOMINATION"
                   <> help "Dénomination sociale de la personne morale ayant donné accord"))


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


toMesuresTypeCode :: String -> MFI_T.MesuresTypeCode
toMesuresTypeCode "COURBES" = MFI_T.MesuresTypeCodeCOURBES
toMesuresTypeCode "ENERGIE" = MFI_T.MesuresTypeCodeENERGIE
toMesuresTypeCode "PMAX"    = MFI_T.MesuresTypeCodePMAX
toMesuresTypeCode "INDEX"   = MFI_T.MesuresTypeCodeINDEX
toMesuresTypeCode s         = errorWithoutStackTrace $ "Type inconnu: " ++ s ++ " (COURBES|ENERGIE|PMAX|INDEX)"

toMesuresCorrigees :: Bool -> MFI_T.MesuresCorrigees
toMesuresCorrigees b = MFI_T.MesuresCorrigees b

toSensMFI :: String -> MFI_T.Sens
toSensMFI "SOUTIRAGE" = MFI_T.SensSOUTIRAGE
toSensMFI "INJECTION" = MFI_T.SensINJECTION
toSensMFI s           = errorWithoutStackTrace $ "Sens inconnu: " ++ s ++ " (SOUTIRAGE|INJECTION)"

toCadreMFI :: String -> MFI_T.CadreAcces
toCadreMFI "ACCORD_CLIENT" = MFI_T.CadreAccesACCORDCLIENT
toCadreMFI "SERVICE_ACCES" = MFI_T.CadreAccesSERVICEACCES
toCadreMFI s               = errorWithoutStackTrace $ "CadreAcces inconnu: " ++ s ++ " (ACCORD_CLIENT|SERVICE_ACCES)"

toSensMFA :: String -> MFA_T.Sens
toSensMFA "SOUTIRAGE" = MFA_T.Sens_SOUTIRAGE
toSensMFA "INJECTION" = MFA_T.Sens_INJECTION
toSensMFA s           = errorWithoutStackTrace $ "Sens inconnu: " ++ s ++ " (SOUTIRAGE|INJECTION)"

toCadreMFA :: String -> MFA_T.CadreAcces
toCadreMFA "ACCORD_CLIENT" = MFA_T.CadreAcces_ACCORD_CLIENT
toCadreMFA "SERVICE_ACCES" = MFA_T.CadreAcces_SERVICE_ACCES
toCadreMFA "EST_TITULAIRE" = MFA_T.CadreAcces_EST_TITULAIRE
toCadreMFA s               = errorWithoutStackTrace $ "CadreAcces inconnu: " ++ s ++ " (ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE)"

toSensITC :: String -> ITC_T.Sens
toSensITC "SOUTIRAGE" = ITC_T.Sens_SOUTIRAGE
toSensITC "INJECTION" = ITC_T.Sens_INJECTION
toSensITC s           = errorWithoutStackTrace $ "Sens inconnu: " ++ s ++ " (SOUTIRAGE|INJECTION)"

toCadreITC :: String -> ITC_T.CadreAcces
toCadreITC "ACCORD_CLIENT" = ITC_T.CadreAcces_ACCORD_CLIENT
toCadreITC "SERVICE_ACCES" = ITC_T.CadreAcces_SERVICE_ACCES
toCadreITC "EST_TITULAIRE" = ITC_T.CadreAcces_EST_TITULAIRE
toCadreITC s               = errorWithoutStackTrace $ "CadreAcces inconnu: " ++ s ++ " (ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE)"

toSensAcces :: String -> ACCES.Sens
toSensAcces "SOUTIRAGE" = ACCES.SensSOUTIRAGE
toSensAcces "INJECTION" = ACCES.SensINJECTION
toSensAcces s           = errorWithoutStackTrace $ "Sens inconnu: " ++ s ++ " (SOUTIRAGE|INJECTION)"


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

    M023 sub -> case sub of

        M023Fines o -> do
            myType <- MFI.initType
                        (mfiPoints o)
                        (toMesuresTypeCode (mfiType o))
                        (toMesuresCorrigees <$> mfiCorrigees o)
                        (mfiDebut o) (mfiFin o)
                        (toSensMFI (mfiSens o))
                        (toCadreMFI (mfiCadre o))
            if xml
              then MFI.xmlRequest myType >>= (putStrLn . prettyXml)
              else do
                rep <- MFI.wsRequest myType :: IO (Either (String, String) MFI_T.AffaireId)
                if raw then pPrint rep
                       else renderApp (fmap (AffaireIdResult . simpleTypeText) rep)

        M023Facturantes o -> do
            myType <- MFA.initType
                        (mfaPoints o)
                        (mfaDebut o) (mfaFin o)
                        (toSensMFA (mfaSens o))
                        (toCadreMFA (mfaCadre o))
            if xml
              then MFA.xmlRequest myType >>= (putStrLn . prettyXml)
              else do
                rep <- MFA.wsRequest myType :: IO (Either (String, String) MFA_T.AffaireId)
                if raw then pPrint rep
                       else renderApp (fmap (AffaireIdResult . simpleTypeText) rep)

        M023ITC o -> do
            myType <- ITC.initType
                        (itcPoints o)
                        (toSensITC (itcSens o))
                        (toCadreITC (itcCadre o))
            if xml
              then ITC.xmlRequest myType >>= (putStrLn . prettyXml)
              else do
                rep <- ITC.wsRequest myType :: IO (Either (String, String) ITC_T.AffaireId)
                if raw then pPrint rep
                       else renderApp (fmap (AffaireIdResult . simpleTypeText) rep)


    Acces o -> do
        let accordType = case accesAccord o of
                AccesPhysique nom -> ACCES.AccordPersonnePhysiqueNom nom
                AccesMorale   den -> ACCES.AccordPersonneMoraleDenominationSociale den
        myType <- ACCES.initType
                    (accesPoint o)
                    (acesDuree o)
                    accordType
                    (accesType o)
                    (toSensAcces (accesSens o))
        if xml
          then ACCES.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- ACCES.wsRequest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
            if raw then pPrint rep else renderApp rep


main :: IO ()
main = docommand =<< execParser optsHeader
  where
    optsHeader = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Consultation des webservices SGE Enedis"
     <> header "conso-elec-sge" )
