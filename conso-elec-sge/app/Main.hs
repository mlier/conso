{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Options.Applicative.Help.Pretty (vsep, fillSep, Doc, pretty, align, indent, fill)
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

import qualified Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10      as RSSM
import           Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type  (RechercherServicesSouscritsMesuresResponseType)
import           Display.ServicesSouscritsDisplay                              ()   -- instance Renderable

import qualified Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10     as ARRET
import           Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type (CommanderArretServiceSouscritMesuresResponseType)
import           Display.ArretDisplay                                           ()   -- instance Renderable

import qualified Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30      as CCPM
import           Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type  (CommanderCollectePublicationMesuresResponseType)
import           Display.CollecteDisplay                                         ()   -- instance Renderable


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
    | Services ServicesOptions
    | Arret ArretOptions
    | Collecte CollecteOptions
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

data ArretOptions = ArretOptions
  { arretPoint   :: String
  , arretService :: String
  } deriving (Eq, Show)

newtype ServicesOptions = ServicesOptions
  { servicesPoint :: String
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

data CollecteOptions = CollecteOptions
  { collectePoint      :: String
  , collecteDuree      :: Maybe Integer
  , collecteType       :: String
  , collecteSens       :: String
  , collecteRecurrente :: Bool
  , collecteCorrigees  :: Maybe Bool
  , collectePeriode    :: Maybe String
  , collecteAccord     :: AccesAccordOpts
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
                ( progDesc $ unlines [
                      "ConsulterDonneesTechniquesContractuellesV10 : "
                    , "Obtenir des informations techniques et contractuel sur un point." ] )
            )
        <> command "recherche"
            ( info
                ( Recherche <$> rechercheParser <**> helper )
                ( progDesc $ unlines [
                      "RechercherPointV20 : Rechercher des points par critères (adresse, nom, domaine…)"
                    , "" ] )
            )
        <> command "mesures"
            (info
                ( Mesures <$> mesuresParser <**> helper )
                (  fullDesc
                <> progDesc "ConsulterMesuresV11 : Avoir des mesures mensuelles" 
                <> footerDoc (Just aideMesuresDetaillee)
                )
            )
        <> command "mesuresdetail"
            ( info
                ( MesuresDetail <$> mesuresDetailComm <**> helper )
                (    fullDesc
                  <> progDesc "ConsulterMesuresDetailleesV3 : Accès complet aux courbes, pmax, index et énergies"
                  <> footerDoc (Just aideMesuresDetailDetaillee)
                )
            )
        <> command "m023"
            ( info
                ( M023 <$> m023Parser <**> helper )
                (    fullDesc
                  <> progDesc "M023 : Demander publication de données M023 (fines|facturantes|itc)"
                  <> footerDoc (Just aideM023GlobalDetaillee)
                )
            )
        <> command "services"
            ( info
                ( Services <$> servicesParser <**> helper )
                ( fullDesc
                <> progDesc "RechercherServicesSouscritsMesuresV10 : Rechercher les services souscrits de mesures sur un point"
                <> footerDoc ( Just aideServicesDetaillee )
                )
            )
        <> command "acces"
            ( info
                ( Acces <$> accesParser <**> helper )
                (  fullDesc 
                <> progDesc "CommanderAccesDonneesMesuresV10 : Commander l'accès aux données de mesures (AME)"
                <> footerDoc ( Just aideAccesDetaillee )
                )
            )
        <> command "collecte"
            ( info
                ( Collecte <$> collecteParser <**> helper )
                ( fullDesc 
                <> progDesc "CommanderCollectePublicationMesuresV30 : Commander la collecte ou la publication de mesures (AME/CDC/IDX)" 
                <> footerDoc ( Just aideCollecteDetaillee )
                )
            )
        <> command "arret"
            ( info
                ( Arret <$> arretParser <**> helper )
                ( fullDesc 
                <> progDesc "CommanderArretServiceSouscritMesuresV10 : Commander l'arrêt d'un service souscrit de mesures (ASS)" 
                <> footerDoc ( Just aideArretDetaillee )
                )
            )
        )

aideMesuresDetaillee :: Doc
aideMesuresDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "Le service ConsultationMesures prend en entrée l’identifiant du point (PRM)"
                     , "et retourne en réponse les mesures demandées. Les Fournisseurs non titulaires ou les Tiers"
                     , "doivent préciser avoir l’autorisation du client actuel."
                     ]
    , pretty ("" :: String)
    , para $ unwords [ "Les mesures retournées sont des consommations restituées sur la grille Distributeur et/ou"
                     , "la grille Fournisseur selon le tableau ci-dessous, sur une profondeur maximale de trente-six"
                     , "mois limitée par la date de dernière mise en service :" ]
    , pretty ("" :: String)
    , indent 2 $ vsep
        [ row3 20 "Segment"           "Grille Dist." "Grille Fourn."
        , row3 20 "-------"           "------------" "-------------"
        , row3 20 "C1*"              "Oui"          "Non"
        , row3 20 "C2-C4"            "Oui"          "Si définie"
        , row3 20 "C5 Nouvelle"      "Oui"          "Oui"
        , row3 20 "C5 Ancienne"      "Non"          "Oui"
        ]
        , pretty ("" :: String)
        , pretty ("* Le service n’est pas disponible pour les points C1 raccordés en HTA." :: String)
        , pretty ("" :: String)
        , para $ unwords [ "Pour les points du segment C5 migrés dans la nouvelle chaîne, pour chacune des deux grilles,"
                         , "des séries de mesures datées par classe temporelle et par calendrier sont restituées."
                         , "Chaque série comporte :" ]
        , indent 2 $ vsep
            [ bullet "la grandeur physique : Énergie Active (EA)"
            , bullet "la classe temporelle et le calendrier"
            , bullet "l’unité de mesure (kWh)"
            , bullet $ unwords [ "la liste des mesures datées, triées dans l’ordre anté-chronologique comprenant : valeur,"
                               , "dates de début/fin, statut (initiale, annulée ou rectifiée), nature et événement déclencheur." ]
            ]
        , pretty ("" :: String)
        , para "Les séries de mesures sont triées par calendrier (du plus récent au plus ancien) puis par classe temporelle."
        , pretty ("" :: String)
        , para $ unwords [ "Pour les autres points, pour chacune des grilles transmises, des séries de mesures par"
                         , "classe temporelle sont restituées. Chaque série comporte :" ]
        , indent 2 $ vsep
            [ bullet "la grandeur physique : Énergie Active (EA)"
            , bullet "la classe temporelle"
            , bullet "l’unité de mesure (kWh)"
            , bullet $ unwords [ "la liste des mesures datées, triées dans l’ordre anté-chronologique comprenant : valeur,"
                               , "dates de début/fin, statut (initiale, annulée ou rectifiée), nature et événement déclencheur." ]
            ]
    ]

aideMesuresDetailDetaillee :: Doc
aideMesuresDetailDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "Le service consulterMesuresDetaillees-V3 permet aux acteurs de marché de consulter"
                     , "depuis leur SI les données de mesure détaillées d’un point C1-C4, P1-P3 ou d’un"
                     , "point C5 ou P4 équipé d’un compteur Linky communicant et ouvert aux services"
                     , "(niveau d’ouverture aux services = 2)." ]
    , pretty ("" :: String)
    , para $ unwords [ "L’accord explicite du client est requis à l’exception du cas de demande de"
                     , "consultation de la puissance maximale mensuelle par le fournisseur titulaire." ]
    , pretty ("" :: String)
    , pretty ("Les données consultables sont les suivantes :" :: String)
    , indent 2 $ vsep
        [ -- --- SEGMENT C5 ---
          bullet "Pour les points C5 Linky ouverts aux services (niveau d’ouverture = 2) :"
        , indent 4 $ vsep
            [ pretty ("- consommations globales quotidiennes," :: String)
            , para "- puissances maximales quotidiennes et mensuelles (y compris facette soutirage d’un P4 surplus, et les Pmax par phase des compteurs triphasés),"
            , pretty ("- index quotidiens," :: String)
            , pretty ("- courbe de puissance active au pas enregistré." :: String)
            ]
        , pretty ("" :: String)
        
        -- --- SEGMENT P4 ---
        , bullet "Pour les points P4 Linky ouverts aux services (niveau d’ouverture = 2) :"
        , indent 4 $ vsep
            [ pretty ("- énergies actives globales quotidiennes produites," :: String)
            , pretty ("- puissances maximales quotidiennes et mensuelles," :: String)
            , pretty ("- index quotidiens," :: String)
            , pretty ("- courbe de charge au pas enregistré." :: String)
            ]
        , pretty ("" :: String)
        
        -- --- SEGMENT C1-C4 ---
        , bullet "Pour les points C1-C4 et P1-P3 (selon les mesures disponibles pour chaque point) :"
        , indent 4 $ vsep
            [ pretty ("- courbe de puissance active au pas enregistré," :: String)
            , pretty ("- courbe de puissance réactive inductive ou capacitive au pas enregistré," :: String)
            , pretty ("- courbe de tension au pas enregistré," :: String)
            , pretty ("- index quotidiens en consommation, ou en production selon la facette," :: String)
            , pretty ("- énergies globales quotidiennes en consommation, ou produites selon la facette." :: String)
            ]
        ]
    ]

aideM023GlobalDetaillee :: Doc
aideM023GlobalDetaillee = vsep
    [ para "Trois services sont mis à disposition pour les demandes M023 :"
    , indent 2 $ vsep
        [ bullet "Le service commandeHistoriqueDonneesMesuresFines"
        , bullet "Le service commandeHistoriqueDonneesMesuresFacturantes"
        , bullet "Le service commandeInformationsTechniquesEtContractuelles"
        ]
    , pretty ("" :: String)
    , para $ unwords [ "Le tableau ci-dessous indique les types de données et les segments disponibles"
                     , "pour chaque service (L2 = Niveau d'ouverture 2, L1 = Niveau <= 1) :" ]
    , pretty ("" :: String)
    , indent 2 $ vsep
        [ row4 35 "TYPE DE DONNÉES"             "C1-P3"  "C5/P4 L2" "C5/P4 L1"
        , row4 35 "----------------"             "-----"  "--------" "--------"
        , row4 35 "Fines (Courbes, Index, Énerg.)" "Oui"    "Oui"      "Non"
        , row4 35 "Fines (Puissances Max)"         "Non"    "Oui"      "Non"
        , row4 35 "Mesures Facturantes"           "Oui"    "Oui"      "Oui"
        , row4 35 "Données Tech. & Contract."     "Oui"    "Oui"      "Oui"
        ]
    , pretty ("" :: String)
    , para "Note : L'accès aux mesures fines nécessite un niveau d'ouverture aux nouveaux services égal à 2."
    ]

aideAccesDetaillee :: Doc
aideAccesDetaillee = vsep
    [ pretty ("" :: String)
    , pretty ("Le service CommandeAccesDonneesMesures permet aux acteurs de marché de demander :" :: String)
    , indent 2 $ vsep
        [ bullet "un accès aux données de courbe de charge pour un point C1-C5 ou P1-P4,"
        , bullet "un accès aux données d’énergies globales quotidiennes pour un point C1-C5 ou P1-P3,"
        , bullet "un accès aux données de puissances maximales quotidiennes pour un point C5 ou facette C5 d’un point P4 en surplus,"
        , bullet "un accès aux données d’index quotidiens pour un point C1-C5 ou P1-P4,"
        , bullet "le renouvellement d’un service actif."
        ]
    , vsep
        [ pretty ("" :: String)
        , pretty ("Le service autorise pour l’ensemble des acteurs du marché un maximum de 10 demandes par seconde." :: String)
        , pretty ("Au-delà les demandes sont non recevables. Le service est optimisé pour 1000 demandes par jour." :: String)
        , pretty ("Au-delà de cette limite, le délai de réponse ne peut être garanti et les demandes sont exposées" :: String)
        , pretty ("à l’atteinte du délai maximum de traitement." :: String)
        ]
    ]

aideServicesDetaillee :: Doc
aideServicesDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "Le service RechercheServicesSouscritsMesures permet à un acteur de marché (fournisseur ou tiers)"
                     , "de récupérer les données des services souscrits de mesures qu’il a commandés sur un point."
                     , "Il peut faire cette demande sur un point C5 (ancienne ou nouvelle chaîne) ou sur un point C1-C4"
                     , "ou P1-P3, qu’il soit titulaire du point ou non."
                     ]
    , pretty ("" :: String) 
    , para $ unwords [ "Ce service retourne les informations des services de collecte ou transmission récurrente de"
                     , "données de mesures que l’acteur de marché demandeur a lui-même souscrits pour ce point, qu’ils"
                     , "soient en cours de demande, actifs ou terminés." 
                     ]
    , pretty ("" :: String)
    , para $ unwords [ "Ce service retourne aussi pour le segment C5 le service souscrit d’opposition à l’enregistrement"
                     , "de la courbe de charge s’il a été demandé (visible pour tous les acteurs de marché, que ce soit"
                     , "le fournisseur titulaire ou non)." 
                     ]
    ]

aideCollecteDetaillee :: Doc
aideCollecteDetaillee = vsep
    [ pretty ("" :: String)
    , pretty ("Le service CommandeCollectePublicationMesures permet aux acteurs de marché :" :: String)
    , indent 2 $ vsep
        [ bullet $ unwords [ "d’activer la collecte de la courbe de charge pour un point C5 ou P4 avec un niveau d’ouverture"
                           , "aux services égal à 2, au pas de collecte par défaut à 30 minutes," ]
        , bullet $ unwords [ "de demander la transmission récurrente de la courbe de charge pour un point C5 avec un niveau"
                           , "d’ouverture aux services égal à 2, au pas de collecte par défaut à 30 minutes," ]
        , bullet $ unwords [ "de demander la transmission récurrente des index quotidiens et des Pmax quotidiennes pour un"
                           , "point C5 avec un niveau d’ouverture aux services égal à 2," ]
        , bullet $ unwords [ "d’activer la collecte de courbes enrichies (de charge et de tension) pour un point C1-C4 ou"
                           , "P1-P3, au pas de collecte par défaut à 5 minutes," ]
        , bullet $ unwords [ "de demander la transmission récurrente de courbes enrichies (de charge et de tension) pour un"
                           , "point C1-C4 ou P1-P3, au pas de collecte par défaut à 5 minutes," ]
        , bullet $ unwords [ "de demander la transmission quotidienne des index et des autres données du compteur pour un point C1-C4"
                           , "ou P1-P3 (une demande concerne les données injection ou soutirage). Le service souscrit inclut également"
                           , "la transmission des données sur glissement à chaque changement de période contractuelle (cyclique et"
                           , "lors d’une modification contractuelle)."
                           ]
        , bullet "de demander le renouvellement d’un service déjà actif."
        ]
    ]

aideArretDetaillee :: Doc
aideArretDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "Le service CommandeArretServiceSouscritMesures permet à un Fournisseur ou un Tiers"
                     , "de demander à Enedis sur un PRM des segments C1 à C5 et P4, l’arrêt d’un service"
                     , "souscrit de collecte ou transmission récurrente de données de mesures dont il est bénéficiaire."
                     ]
    , pretty ("" :: String)
    , para $ unwords [ "Ce service permet aussi au Fournisseur titulaire d’un PRM C5, de demander à Enedis"
                     , "pour son client, la levée de l’opposition à l’enregistrement de la courbe de charge sur le compteur."
                     ]
    , pretty ("" :: String)
    , para $ unwords [ "Le demandeur peut demander l’arrêt d’un service souscrit sur un point C5 ou P4"
                     , "ou sur un point C1-C4." 
                     ]
    , pretty ("" :: String)
    , para $ unwords [ "L’objet de la demande (valeur de la balise objetCode dans donneesGenerales) correspondant"
                     , "au service d’Arrêt d’un Service Souscrit de mesures est ASS."
                     ]
    ]

infParser :: Parser InfoOptions
infParser = InfoOptions
      <$> strOption
          ( long "point"
         <> short 'p'
         <> metavar "POINT"
         <> help "Identifiant PRM du point de mesure sélectionné" )
      <*> switch
          ( long "autorisation"
         <> short 'a'
         <> help "Avec ou sans information contractuelle, nécessite l'autorisation client (default: sans autorisation)" )

mesuresParser :: Parser MesuresOptions
mesuresParser = MesuresOptions
      <$> strOption
          ( long "point"
         <> short 'p'
         <> metavar "POINT"
         <> help "Identifiant PRM du point de mesure sélectionné" )

-- | Options communes aux 4 sous-commandes ; le metavar de --grandeur est spécifique à chaque type.
mdCommonParser :: String -> Parser MdCommonOpts
mdCommonParser grandeurMeta = MdCommonOpts
      <$> strOption ( long "point"    <> short 'p' <> metavar "PRM"
                   <> help "Identifiant PRM du point de mesure sélectionné" )
      <*> strOption ( long "grandeur" <> short 'g' <> metavar grandeurMeta
                   <> help "Grandeurs physiques disponibles" )
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
               (  fullDesc
               <> progDesc "Obtenir la courbe de charge (PA, PRI, PRC, E, TOUT)"
               <> footerDoc (Just aideCourbeDetaillee)
               )
        )
    <> command "pmax"
        ( info ( MdPmax
                   <$> mdCommonParser "PMA|TOUT"
                   <*> strOption ( long "pas" <> metavar "P1D|P1M"
                                <> value "P1D" <> showDefault
                                <> help "Pas temporel quotidien ou mensuel" )
                   <**> helper )
               ( fullDesc
               <> progDesc "Obtenir la puissance maximale (Pmax) quotidienne ou mensuelle"
               <> footerDoc (Just aidePmaxDetaillee)
               )
        )
    <> command "energie"
        ( info ( MdEnergie <$> mdCommonParser "EA|ERC|ERI" <**> helper )
               (  fullDesc
               <> progDesc "Obtenir l'énergie globale quotidienne (EA, ERC, ERI)"
               <> footerDoc (Just aideEnergieDetaillee)
               )
        )  
    <> command "index"
        ( info ( MdIndex <$> mdCommonParser "EA|ER|ERC|ERI|DD|DE|DQ|PMA|TF|TOUT" <**> helper )
               (  fullDesc
               <> progDesc "Obtenir les index d'une grandeur physique (EA, ER, etc.)"
               <> footerDoc (Just aideIndexDetaillee)
               )
        )
    )

aideCourbeDetaillee :: Doc
aideCourbeDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "La Courbe De Charge (CDC) permet la relève des données de soutirage/injection"
                     , "à une maille plus fine (infra-journalière). Chaque « point de courbe de charge »"
                     , "correspond à la puissance moyenne constatée sur un pas de temps précis." ]
    , pretty ("" :: String)
    , para "Le service restitue des courbes de charge qui peuvent être :"
    , indent 2 $ vsep
        [ bullet "En puissance active, dont l’unité est le Watt (W)."
        , bullet "En puissance réactive inductive ou capacitive, dont l’unité est le VoltAmpère Réactif (VAr)."
        , bullet $ unwords [ "En tension, dont l’unité est le Volt (V). La courbe de tension est la seule"
                           , "qui soit indépendante de la grandeur métier (CONS/PROD)." ]
        ]
    , pretty ("" :: String)
    , para "Codes des grandeurs disponibles :"
    , indent 2 $ vsep
        [ row3 10 "PA"   "Puissance Active (W)"        "(Seule dispo pour C5/P4)"
        , row3 10 "PRI"  "Puissance Réactive Inductive" "(VAr)"
        , row3 10 "PRC"  "Puissance Réactive Capacitive" "(VAr)"
        , row3 10 "E"    "Tension (V)"                 "(Indépendante CONS/PROD)"
        , row3 10 "TOUT" "Ensemble des courbes disponibles" ""
        ]
    ]

aidePmaxDetaillee :: Doc
aidePmaxDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "La donnée « Pmax quotidienne » se découpe en deux informations remontées"
                    , "régulièrement pour chaque point : la Puissance Maximale atteinte sur une journée J"
                    , "sur ce point, ainsi que l’horodate où cette puissance a été atteinte." ]
    , pretty ("" :: String)
    , para $ unwords [ "La donnée de Pmax quotidienne est remontée une fois par jour pour chaque grandeur"
                    , "« physique », dépendant de l’installation de comptage :" ]
    , indent 2 $ vsep
        [ bullet $ unwords [ "Une Pmax quotidienne de soutirage (compteur monophasé et triphasé Linky C5)."
                        , "Pour les compteurs triphasés, il s’agit de la valeur maximale de la somme des"
                        , "puissances des trois phases." ]
        , bullet "Une Pmax quotidienne en injection (compteur Linky P4)."
        , bullet "Une Pmax quotidienne de soutirage par phase pour les compteurs Linky triphasés."
        ]
    , pretty ("" :: String)
    , para "La puissance maximale est une puissance apparente, dont l’unité est le Voltampère (VA)."
    , pretty ("" :: String)
    , para "Options de l'argument principal :"
    , indent 2 $ vsep
        [ bullet "PMA : récupération de la Pmax monophasée ou 'équivalente monophasé' (somme des 3 phases)."
        , bullet "TOUT : récupération de l'ensemble (équivalent monophasé + Pmax par phase)."
        ]
    ]
 
aideEnergieDetaillee :: Doc
aideEnergieDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "L’énergie globale quotidienne (aussi simplement appelée énergie quotidienne) est le calcul"
                     , "de l’énergie transitant sur le réseau sur une journée, en soutirage (consommation) ou en"
                     , "injection (production)." ]
    , pretty ("" :: String)
    , para $ unwords [ "Les énergies mises à disposition par ce service sont « globales » au sens où elles ne"
                     , "distinguent pas la consommation/production par calendrier ou classe temporelle." ]
    , pretty ("" :: String)
    , para "Spécificités du calcul par segment :"
    , indent 2 $ vsep
        [ bullet $ unwords [ "Segments C5/P4 : Calcul réalisé par différence d’index totalisateurs relevés à minuit."
                           , "Énergies exclusivement actives, dont l’unité est le WattHeure (Wh)." ]
        , bullet $ unwords [ "Segments C1-C4 / P1-P3 : Calcul réalisé par intégrale de courbe de charge."
                           , "Énergies actives (Wh) ou réactives (inductives ou capacitives), dont l’unité est le"
                           , "VoltAmpère Réactif Heure (VArh)." ]
        ]
    , pretty ("" :: String)
    , para "Codes des grandeurs disponibles :"
    , indent 2 $ vsep
        [ row3 10 "EA"   "Énergie Active" "(Wh)"
        , row3 10 "ERC"  "Énergie Réactive Capacitive" "(VArh - C1-C4 uniquement)"
        , row3 10 "ERI"  "Énergie Réactive Inductive"  "(VArh - C1-C4 uniquement)"
        ]
    ]

aideIndexDetaillee :: Doc
aideIndexDetaillee = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "Sur Linky, les index totalisateurs permettent de comptabiliser l’énergie"
                     , "durant l’intégralité de la période de fonctionnement du compteur."
                     , "À l’inverse, les calendriers fournisseurs et distributeurs permettent"
                     , "de définir des classes temporelles pour avoir accès à des index avec"
                     , "une plage de fonctionnement spécifique." ]
    , pretty ("" :: String)
    , para $ unwords [ "Par exemple : Une classe temporelle « heure pleine » dont l’index ne"
                     , "comptabilise l’énergie que durant sa plage de fonctionnement et une"
                     , "classe temporelle « heure creuse » sur l’autre plage de fonctionnement." ]
    , pretty ("" :: String)
    , para $ unwords [ "Chaque classe temporelle correspond à un « cadran » (affiché sur le compteur,"
                     , "par exemple) permettant de faire la correspondance des données du SI avec"
                     , "le compteur physique." ]
    , pretty ("" :: String)
    , para $ unwords [ "Pour les segments C5/P4, les données d’index retournées correspondent aux"
                     , "données d’index en énergie active (EA), et en énergie réactive (ER) pour les P4." ]
    , pretty ("" :: String)
    , para $ unwords [ "Pour les segments C1-C4 et P1-P3, les services d’index permettent également"
                     , "l’accès à d’autres données du compteur (voir guide Enedis.SGE.GUI.0502.Flux.R6X)." ]
    , pretty ("" :: String)
    , para "Codes des grandeurs disponibles en argument :"
    , indent 2 $ vsep
        [ row3 10 "EA"   "Énergie Active" ""
        , row3 10 "ER"   "Énergie Réactive" "(P4 uniquement)"
        , row3 10 "ERC"  "Énergie Réactive Capacitive" ""
        , row3 10 "ERI"  "Énergie Réactive Inductive" ""
        , row3 10 "DD"   "Durée de Dépassement" ""
        , row3 10 "DE"   "Dépassement Énergétique" ""
        , row3 10 "DQ"   "Dépassement Quadratique" ""
        , row3 10 "PMA"  "Puissance Maximale Atteinte" ""
        , row3 10 "TF"   "Temps de Fonctionnement" ""
        , row3 10 "TOUT" "Ensemble des données disponibles" ""
        ]
    ]
 
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
               (    fullDesc 
                 <> progDesc "DemandePublicationMesuresFinesM23V10 : Flux R63–R66 pour obtenir des mesures fines."
                 <> footerDoc (Just aideFinesVerbatim)
               )
        )
    <> command "facturantes"
        ( info ( M023Facturantes <$> mfaParser <**> helper )
               (    fullDesc
                 <> progDesc "DemandePublicationMesuresFacturantesM23V10 : Flux R67 de mesures facturantes."
                 <> footerDoc (Just aideFacturantesVerbatim)
               )
        )
    <> command "itc"
        ( info ( M023ITC <$> itcParser <**> helper )
               (    fullDesc
                 <> progDesc "DemandePublicationInformationsTechniquesContractuellesM23V10 : Flux C68 (ITC)."
                 <> footerDoc (Just aideITCVerbatim)
               )
        )
    )

aideFinesVerbatim :: Doc
aideFinesVerbatim = vsep
    [ pretty ("" :: String)
    -- BLOC R63
    , para $ unwords [ "Flux R63 COURBES : La Courbe De Charge permet la relève des données de soutirage/injection"
                     , "à une maille plus fine (infra-journalière). Chaque point de courbe de charge correspond"
                     , "à la puissance moyenne constatée sur un pas de temps précis. Le service restitue des"
                     , "courbes de charge qui peuvent être :" ]
    , indent 2 $ vsep 
        [ pretty ("— En puissance active, dont l’unité est le Watt (W)." :: String)
        , pretty ("— En puissance réactive inductive ou capacitive, dont l’unité est le VoltAmpère Réactif (VAr)" :: String)
        , pretty ("— En tension, dont l’unité est le Volt (V). La courbe de tension est la seule qui soit indépendante de la grandeur métier (CONS/PROD)." :: String)
        ]
    , para "Profondeur maximale de l’historique : 24 derniers mois par rapport à la date du jour, limités à la dernière mise en service"
    , para "Nombre de PRM max par demande JSON : 1500"
    , pretty ("" :: String)
    
    -- BLOC R64
    , para $ unwords [ "Flux R64 INDEX : Sur Linky, Les index totalisateurs permettent de comptabiliser l’énergie"
                     , "durant l’intégralité de la période de fonctionnement du compteur. À l’inverse, les calendriers"
                     , "fournisseurs et distributeurs permettent de définir des classes temporelles pour avoir accès"
                     , "à des index avec une plage de fonctionnement spécifique. Par exemple : Une classe temporelle"
                     , "\"heure pleine\" dont l’index ne comptabilise l’énergie que durant sa plage de fonctionnement"
                     , "et une classe temporelle \"heure creuse\" sur l’autre plage de fonctionnement. Chaque classe"
                     , "temporelle correspond à un \"cadran\" (affiché directement sur le compteur Linky, par exemple)"
                     , "permettant de faire la correspondance des données du SI avec le compteur physique. Pour les"
                     , "segments C5/P4, les données d’index retournées correspondent aux données d’index en énergie"
                     , "active, et en énergie réactive pour les P4 uniquement. Pour les segments C1-C4 et P1-P3,"
                     , "les services d’index permettent également l’accès à d’autres données du compteur." ]
    , para "Profondeur maximale de l’historique : 36 mois par rapport à la date du jour, limités à la dernière mise en service"
    , para "Nombre de PRM max par demande JSON : 1500"
    , pretty ("" :: String)

    -- BLOC R65
    , para $ unwords [ "Flux R65 ENERGIE : L’énergie globale quotidienne (aussi simplement appelée énergie quotidienne)"
                     , "est le calcul de l’énergie transitant sur le réseau sur une journée, en soutirage (consommation)"
                     , "ou en injection (production). Les énergies mises à disposition par ce service sont « globales »"
                     , "au sens où elles ne distinguent pas la consommation/production par calendrier ou classe"
                     , "temporelles. Pour les points C5/P4, le calcul de l’énergie quotidienne est réalisé par différence"
                     , "d’index totalisateurs relevés à minuit. Les énergies globales quotidiennes restituées sont"
                     , "exclusivement des énergies actives, dont l’unité est le WattHeure (Wh). Pour les points C1-C4/P1-P3,"
                     , "le calcul de l’énergie quotidienne est réalisé par intégrale de courbe de charge. Les énergies"
                     , "globales quotidiennes restituées peuvent être actives (en Wh), ou réactives (inductives ou"
                     , "capacitives), dont l’unité est le VoltAmpère Réactif Heure (VArh)." ]
    , para "Profondeur maximale de l’historique : 36 mois par rapport à la date du jour, limités à la dernière mise en service"
    , para "Nombre de PRM max par demande JSON : 10 000"
    , pretty ("" :: String)

    -- BLOC R66
    , para $ unwords [ "Flux R66 PMAX : La donnée \"Pmax quotidienne\" se découpe en deux informations remontées"
                     , "régulièrement pour chaque point : la Puissance Maximale atteinte sur une journée J sur ce point,"
                     , "ainsi que l’horodate où cette puissance a été atteinte. La donnée de Pmax quotidienne est"
                     , "remontée une fois par jour pour chaque grandeur \"physique\", dépendant de l’installation"
                     , "de comptage :" ]
    , indent 2 $ vsep
        [ pretty ("- Une Pmax quotidienne de soutirage (compteur monophasé et triphasé Linky C5). Pour les compteurs triphasés, il s’agit de la valeur maximale de la somme des puissances des trois phases" :: String)
        , pretty ("- Une Pmax quotidienne en injection2, (compteur Linky P4)." :: String)
        , pretty ("- Une Pmax quotidienne de soutirage par phase pour les compteurs Linky triphasés2. La puissance maximale est une puissance apparente, dont l’unité est le Voltampère (VA)." :: String)
        ]
    , para "Profondeur maximale de l’historique : 36 mois par rapport à la date du jour, limités à la dernière mise en service"
    , para "Nombre de PRM max par demande JSON : 10 000"
    ]

aideFacturantesVerbatim :: Doc
aideFacturantesVerbatim = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "DemandePublicationMesuresFacturantesM23V10 : Demande d'un flux R67 de mesures"
                     , "facturantes, c'est-à-dire l’ensemble des mesures transmises dans les flux de relevé"
                     , "dédiés au fournisseur titulaire du point dans le cadre du contrat unique ou au client"
                     , "dans les autres cas." ]
    ]

aideITCVerbatim :: Doc
aideITCVerbatim = vsep
    [ pretty ("" :: String)
    , para $ unwords [ "DemandePublicationInformationsTechniquesContractuellesM23V10 : Demande d'un flux C68"
                     , "d'informations techniques et contractuelles, c'est-à-dire les informations comprennent"
                     , "la situation de comptage, la situation d’alimentation du PRM, l’installation de"
                     , "Production (le cas échéant)." ]
    ]


para :: String -> Doc
para txt = fillSep (map pretty (words txt))

bullet :: String -> Doc
bullet txt = pretty ("• " :: String) <> align (fillSep (map pretty (words txt)))

row3 :: Int -> String -> String -> String -> Doc
row3 n c1 c2 c3 = fill n (pretty c1) <> fill 30 (pretty c2) <> pretty c3

row4 :: Int -> String -> String -> String -> String -> Doc
row4 n c1 c2 c3 c4 = 
    fill n (pretty c1) <> 
    fill 10 (pretty c2) <> 
    fill 12 (pretty c3) <> 
    pretty c4

mfiParser :: Parser MFIOptions
mfiParser = MFIOptions
    <$> some      (strOption (long "point" <> short 'p' <> metavar "PRM"
                             <> help "Identifiant PRM du point de mesure sélectionné"))
    <*> strOption  (long "type"  <> short 't' <> metavar "COURBES|INDEX|ENERGIE|PMAX"
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
                             <> help "Identifiant PRM du point de mesure sélectionné"))
    <*> strOption  (long "debut" <> metavar "YYYY-MM-DD" <> help "Date de début (incluse)")
    <*> strOption  (long "fin"   <> metavar "YYYY-MM-DD" <> help "Date de fin (exclue)")
    <*> strOption  (long "sens"  <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> strOption  (long "cadre" <> metavar "ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE"
                   <> value "ACCORD_CLIENT" <> showDefault <> help "Cadre d'accès")

itcParser :: Parser ITCOptions
itcParser = ITCOptions
    <$> some      (strOption (long "point" <> short 'p' <> metavar "PRM"
                             <> help "Identifiant PRM du point de mesure sélectionné"))
    <*> strOption  (long "sens"  <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> strOption  (long "cadre" <> metavar "ACCORD_CLIENT|SERVICE_ACCES|EST_TITULAIRE"
                   <> value "ACCORD_CLIENT" <> showDefault <> help "Cadre d'accès")


arretParser :: Parser ArretOptions
arretParser = ArretOptions
    <$> strOption (long "point"   <> short 'p' <> metavar "PRM"
                  <> help "Identifiant PRM du point de mesure sélectionné")
    <*> strOption (long "service" <> short 's' <> metavar "SERVICE_ID"
                  <> help "Identifiant du service souscrit à arrêter")


servicesParser :: Parser ServicesOptions
servicesParser = ServicesOptions
    <$> strOption (long "point" <> short 'p' <> metavar "PRM"
                  <> help "Identifiant PRM du point")


accesParser :: Parser AccesOptions
accesParser = AccesOptions
    <$> strOption  (long "point"  <> short 'p' <> metavar "PRM"
                   <> help "Identifiant PRM du point de mesure sélectionné")
    <*> optional   (option auto (long "duree"  <> metavar "JOURS"
                   <> help "Durée de l'accès en jours (max 3×364 pour C5/P4)"))
    <*> strOption  (long "type"   <> short 't' <> metavar "CDC|IDX|ENERGIE|PMAX"
                   <> help "Type de données demandé")
    <*> strOption  (long "sens"   <> metavar "SOUTIRAGE|INJECTION"
                   <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> accesAccordParser

collecteParser :: Parser CollecteOptions
collecteParser = CollecteOptions
    <$> strOption  (long "point"      <> short 'p' <> metavar "PRM"
                    <> help "Identifiant PRM du point de mesure sélectionné")
    <*> optional   (option auto (long "duree" <> metavar "JOURS"
                    <> help "Durée en jours (max 3×365)"))
    <*> strOption  (long "type"       <> short 't' <> metavar "CDC|IDX"
                    <> help "Type de données : CDC (courbe de charge) ou IDX (index/Pmax)")
    <*> strOption  (long "sens"       <> metavar "SOUTIRAGE|INJECTION"
                    <> value "SOUTIRAGE" <> showDefault <> help "Sens de l'énergie")
    <*> switch     (long "recurrente" <> help "Transmission récurrente (sinon : collecte unique)")
    <*> optional   (   flag' True  (long "corrigees" <> help "Courbe corrigée (C1-C4/P1-P3, CDC récurrent)")
                   <|> flag' False (long "brutes"    <> help "Courbe brute"))
    <*> optional   (strOption (long "periodicite" <> metavar "P1D|P7D|P1M"
                    <> help "Périodicité de transmission (CDC récurrent ou IDX)"))
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
toMesuresCorrigees = MFI_T.MesuresCorrigees

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

toSensCCPM :: String -> CCPM.Sens
toSensCCPM "SOUTIRAGE" = CCPM.SensSOUTIRAGE
toSensCCPM "INJECTION" = CCPM.SensINJECTION
toSensCCPM s           = errorWithoutStackTrace $ "Sens inconnu: " ++ s ++ " (SOUTIRAGE|INJECTION)"


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


    Arret o -> do
        myType <- ARRET.initType (arretPoint o) (arretService o)
        if xml
          then ARRET.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- ARRET.wsRequest myType :: IO (Either (String, String) CommanderArretServiceSouscritMesuresResponseType)
            if raw then pPrint rep else renderApp rep

    Services s -> do
        myType <- RSSM.initType (servicesPoint s)
        if xml
          then RSSM.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- RSSM.wsRequest myType :: IO (Either (String, String) RechercherServicesSouscritsMesuresResponseType)
            if raw then pPrint rep else renderApp rep

    Collecte o -> do
        let accordType = case collecteAccord o of
                AccesPhysique nom -> CCPM.AccordPersonnePhysiqueNom nom
                AccesMorale   den -> CCPM.AccordPersonneMoraleDenominationSociale den
        myType <- CCPM.initType
                    (collectePoint o)
                    (collecteDuree o)
                    accordType
                    (collecteType o)
                    (toSensCCPM (collecteSens o))
                    (collecteRecurrente o)
                    (collecteCorrigees o)
                    (collectePeriode o)
        if xml
          then CCPM.xmlRequest myType >>= (putStrLn . prettyXml)
          else do
            rep <- CCPM.wsRequest myType :: IO (Either (String, String) CommanderCollectePublicationMesuresResponseType)
            if raw then pPrint rep else renderApp rep

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
