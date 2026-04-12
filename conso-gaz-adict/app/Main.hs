{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Options.Applicative.Help.Pretty ( vsep, pretty, Doc )
import qualified Data.Text                       as T
import           Data.Text                       ( Text )
import           Text.Pretty.Simple              ( pPrint )

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types
import           Conso.Fr.Gaz.Adict.ConsosPubliees
import           Conso.Fr.Gaz.Adict.ConsosInfos
import           Conso.Fr.Gaz.Adict.DonneesContractuelles
import           Conso.Fr.Gaz.Adict.DonneesTechniques
import           Conso.Fr.Gaz.Adict.InjectionsPubliees
import           Conso.Fr.Gaz.Adict.DroitsAcces
import           Conso.Fr.Gaz.Adict.DroitAcces

import           Display
import           Display.ConsoDisplay        ()
import           Display.DroitsAccesDisplay  ()
import           Display.DonneesDisplay      ()


-- ---------------------------------------------------------------------------
-- Structure des options

data Options = Options
    { optProd    :: Bool
    , optRaw     :: Bool
    , optDebug   :: Bool
    , optCommand :: Command
    } deriving (Show)

data Command
    = Consos        ConsosOpts
    | ConsosInfo    ConsosOpts
    | Injections    ConsosOpts
    | Contrat       ContratOpts
    | Tech          TechOpts
    | Droits
    | DroitsFiltres FiltreOpts
    | Acces         AccesOpts
    | Revoquer      RevoquerOpts
    deriving (Show)

data ConsosOpts = ConsosOpts
    { coPce     :: String
    , coPeriode :: Maybe String
    , coDebut   :: Maybe String
    , coFin     :: Maybe String
    } deriving (Show)

newtype ContratOpts = ContratOpts { ctPce :: String } deriving (Show)
newtype TechOpts    = TechOpts    { techPce :: String } deriving (Show)

data FiltreOpts = FiltreOpts
    { foRole   :: Maybe String
    , foPce    :: Maybe String
    , foStatut :: Maybe String
    , foEtat   :: Maybe String
    } deriving (Show)

data AccesOpts = AccesOpts
    { acPce           :: String
    , acRole          :: String
    , acCp            :: String
    , acNom           :: Maybe String
    , acRaisonSociale :: Maybe String
    , acEmail         :: Maybe String
    , acTel           :: Maybe String
    , acDebutAcces    :: Maybe String
    , acFinAcces      :: Maybe String
    , acDebutConso    :: Maybe String
    , acFinConso      :: Maybe String
    , acContrat       :: Bool
    , acTech          :: Bool
    , acInfos         :: Bool
    , acPubliees      :: Bool
    } deriving (Show)

newtype RevoquerOpts = RevoquerOpts { rvId :: String } deriving (Show)


-- ---------------------------------------------------------------------------
-- Parseurs

opts :: Parser Options
opts = Options
    <$> switch ( long "prod"  <> help "Serveur de production (défaut : bac à sable)" )
    <*> switch ( long "raw"   <> help "Afficher la réponse brute (pPrint)" )
    <*> switch ( long "debug" <> help "Afficher les requêtes HTTP sur stderr" )
    <*> commandParser


commandParser :: Parser Command
commandParser =
    subparser
      (  commandGroup "Données de consommation/injection"
      <> command "consos"
           ( info (Consos <$> consosParser <**> helper)
                  ( fullDesc
                  <> progDesc "Consulter les consommations publiées d'un PCE"
                  <> footerDoc (Just aidePeriode) ) )
      <> command "consos-info"
           ( info (ConsosInfo <$> consosParser <**> helper)
                  ( fullDesc
                  <> progDesc "Consulter les consommations informatives d'un PCE"
                  <> footerDoc (Just aidePeriode) ) )
      <> command "injections"
           ( info (Injections <$> consosParser <**> helper)
                  (progDesc "Consulter les injections publiées d'un PCE") )
      )
    <|> subparser
      (  commandGroup "Données contractuelles et techniques"
      <> command "contrat"
           ( info (Contrat <$> contratParser <**> helper)
                  (progDesc "Consulter les données contractuelles d'un PCE") )
      <> command "tech"
           ( info (Tech <$> techParser <**> helper)
                  (progDesc "Consulter les données techniques d'un PCE") )
      )
    <|> subparser
      (  commandGroup "Droits d'accès"
      <> command "droits"
           ( info (pure Droits <**> helper)
                  (progDesc "Consulter tous mes droits d'accès") )
      <> command "droits-filtres"
           ( info (DroitsFiltres <$> filtreParser <**> helper)
                  (progDesc "Rechercher des droits d'accès avec filtres") )
      <> command "acces"
           ( info (Acces <$> accesParser <**> helper)
                  ( fullDesc
                  <> progDesc "Déclarer un droit d'accès aux données d'un PCE"
                  <> footerDoc (Just aideAcces) ) )
      <> command "revoquer"
           ( info (Revoquer <$> revoquerParser <**> helper)
                  (progDesc "Révoquer un droit d'accès (UUID)") )
      )


consosParser :: Parser ConsosOpts
consosParser = ConsosOpts
    <$> strOption ( long "pce"    <> metavar "PCE" <> help "Identifiant PCE (14 chiffres ou GI+6)" )
    <*> optional (strOption ( long "periode" <> metavar "PERIODE"
                            <> help "Période : 2024, 2024-01, 2024-W03…" ))
    <*> optional (strOption ( long "debut" <> metavar "YYYY-MM-DD" <> help "Date de début" ))
    <*> optional (strOption ( long "fin"   <> metavar "YYYY-MM-DD" <> help "Date de fin" ))


contratParser :: Parser ContratOpts
contratParser = ContratOpts
    <$> strOption ( long "pce" <> metavar "PCE" <> help "Identifiant PCE" )


techParser :: Parser TechOpts
techParser = TechOpts
    <$> strOption ( long "pce" <> metavar "PCE" <> help "Identifiant PCE" )


filtreParser :: Parser FiltreOpts
filtreParser = FiltreOpts
    <$> optional (strOption ( long "role"   <> metavar "ROLE"   <> help "Rôle tiers" ))
    <*> optional (strOption ( long "pce"    <> metavar "PCE"    <> help "Identifiant PCE" ))
    <*> optional (strOption ( long "statut" <> metavar "STATUT" <> help "Statut contrôle preuve" ))
    <*> optional (strOption ( long "etat"   <> metavar "ETAT"   <> help "État du droit d'accès" ))


accesParser :: Parser AccesOpts
accesParser = AccesOpts
    <$> strOption ( long "pce"   <> metavar "PCE" <> help "Identifiant PCE" )
    <*> strOption ( long "role"  <> metavar "ROLE"
                  <> value "AUTORISE_CONTRAT_FOURNITURE"
                  <> showDefault
                  <> help "Rôle tiers" )
    <*> strOption ( long "cp"    <> metavar "CODE_POSTAL" <> help "Code postal du PCE" )
    <*> optional (strOption ( long "nom"    <> metavar "NOM"   <> help "Nom titulaire (personne physique)" ))
    <*> optional (strOption ( long "raison" <> metavar "RS"    <> help "Raison sociale (personne morale)" ))
    <*> optional (strOption ( long "email"  <> metavar "EMAIL" <> help "Email du titulaire" ))
    <*> optional (strOption ( long "tel"    <> metavar "TEL"   <> help "Tél. mobile du titulaire" ))
    <*> optional (strOption ( long "debut-acces" <> metavar "YYYY-MM-DD" <> help "Début droit d'accès" ))
    <*> optional (strOption ( long "fin-acces"   <> metavar "YYYY-MM-DD" <> help "Fin droit d'accès" ))
    <*> optional (strOption ( long "debut-conso" <> metavar "YYYY-MM-DD" <> help "Début périmètre conso" ))
    <*> optional (strOption ( long "fin-conso"   <> metavar "YYYY-MM-DD" <> help "Fin périmètre conso" ))
    <*> switch ( long "contractuelles" <> help "Accès données contractuelles" )
    <*> switch ( long "techniques"     <> help "Accès données techniques" )
    <*> switch ( long "informatives"   <> help "Accès données informatives" )
    <*> switch ( long "publiees"       <> help "Accès données publiées" )


revoquerParser :: Parser RevoquerOpts
revoquerParser = RevoquerOpts
    <$> strOption ( long "id" <> metavar "UUID" <> help "UUID du droit d'accès à révoquer" )


-- ---------------------------------------------------------------------------
-- Messages d'aide

aidePeriode :: Doc
aidePeriode = vsep
    [ pretty ("" :: String)
    , pretty ("Formats de période acceptés :" :: String)
    , pretty ("  --periode 2024          (année entière)" :: String)
    , pretty ("  --periode 2024-06       (mois)" :: String)
    , pretty ("  --periode 2024-W23      (semaine ISO)" :: String)
    , pretty ("  --debut 2024-01-01 --fin 2024-12-31  (plage de dates)" :: String)
    ]

aideAcces :: Doc
aideAcces = vsep
    [ pretty ("" :: String)
    , pretty ("Rôles disponibles :" :: String)
    , pretty ("  AUTORISE_CONTRAT_FOURNITURE (défaut)" :: String)
    , pretty ("  DETENTEUR_CONTRAT_FOURNITURE" :: String)
    , pretty ("  AUTORISE_CONTRAT_INJECTION" :: String)
    , pretty ("  DETENTEUR_CONTRAT_INJECTION" :: String)
    , pretty ("" :: String)
    , pretty ("Exemple :" :: String)
    , pretty ("  conso-gaz-adict acces --pce 12345678901234 --cp 75001 \\" :: String)
    , pretty ("    --raison 'Ma Société SAS' --email client@example.com \\" :: String)
    , pretty ("    --debut-acces 2024-01-01 --fin-acces 2025-01-01 \\" :: String)
    , pretty ("    --debut-conso 2023-01-01 --fin-conso 2025-01-01 \\" :: String)
    , pretty ("    --contractuelles --techniques --informatives --publiees" :: String)
    ]


-- ---------------------------------------------------------------------------
-- Exécution
main :: IO ()
main = do
    o <- execParser $ info (opts <**> helper)
            ( fullDesc
            <> progDesc "Client GRDF API ADICT — consultation des données PCE"
            <> header "conso-gaz-adict — GRDF ADICT B2B v2" )
    session <- initSession (optProd o) (optDebug o)
    run session (optRaw o) (optCommand o)


run :: AdictSession -> Bool -> Command -> IO ()
run session raw cmd = case cmd of

    Consos co -> do
        rep <- consulterConsosPubliees session (packT (coPce co)) (mkPeriode co)
        if raw then pPrint rep else renderApp rep

    ConsosInfo co -> do
        rep <- consulterConsosInfos session (packT (coPce co)) (mkPeriode co)
        if raw then pPrint rep else renderApp rep

    Injections co -> do
        rep <- consulterInjectionsPubliees session (packT (coPce co)) (mkPeriode co)
        if raw then pPrint rep else renderApp rep

    Contrat ct -> do
        rep <- consulterDonneesContractuelles session (packT (ctPce ct)) []
        if raw then pPrint rep else renderApp rep

    Tech t -> do
        rep <- consulterDonneesTechniques session (packT (techPce t))
        if raw then pPrint rep else renderApp rep

    Droits -> do
        rep <- consulterDroitsAcces session
        if raw then pPrint rep else renderApp rep

    DroitsFiltres fo -> do
        let filtre = FiltreAcces
                { fa_role_tiers             = maybe [] (pure . packT) (foRole   fo)
                , fa_id_pce                 = maybe [] (pure . packT) (foPce    fo)
                , fa_statut_controle_preuve = maybe [] (pure . packT) (foStatut fo)
                , fa_etat_droit_acces       = maybe [] (pure . packT) (foEtat   fo)
                }
        rep <- rechercherDroitsAcces session filtre
        if raw then pPrint rep else renderApp rep

    Acces ao -> do
        let demande = DemandeAccesIn
                { din_role_tiers                        = packT (acRole ao)
                , din_raison_sociale                    = fmap packT (acRaisonSociale ao)
                , din_nom_titulaire                     = fmap packT (acNom ao)
                , din_code_postal                       = packT (acCp ao)
                , din_courriel_titulaire                = fmap packT (acEmail ao)
                , din_numero_telephone_mobile_titulaire = fmap packT (acTel ao)
                , din_date_debut_droit_acces            = fmap packT (acDebutAcces ao)
                , din_date_fin_droit_acces              = fmap packT (acFinAcces ao)
                , din_perim_donnees_conso_debut         = fmap packT (acDebutConso ao)
                , din_perim_donnees_conso_fin           = fmap packT (acFinConso ao)
                , din_perim_donnees_inj_debut           = Nothing
                , din_perim_donnees_inj_fin             = Nothing
                , din_perim_donnees_contractuelles      = flagToMaybe (acContrat ao)
                , din_perim_donnees_techniques          = flagToMaybe (acTech ao)
                , din_perim_donnees_informatives        = flagToMaybe (acInfos ao)
                , din_perim_donnees_publiees            = flagToMaybe (acPubliees ao)
                }
        rep <- declarerDroitAcces session (packT (acPce ao)) demande
        if raw then pPrint rep else renderApp rep

    Revoquer rv -> do
        rep <- revoquerDroitAcces session (packT (rvId rv))
        if raw then pPrint rep else renderApp rep


-- ---------------------------------------------------------------------------
-- Petits helpers

packT :: String -> Text
packT = T.pack

mkPeriode :: ConsosOpts -> PeriodeParam
mkPeriode co = case (coPeriode co, coDebut co, coFin co) of
    (Just p, _,      _)      -> ByPeriode   (packT p)
    (_,      Just d, Just f) -> ByDateRange (packT d) (packT f)
    _                        -> ByPeriode "2024"

flagToMaybe :: Bool -> Maybe Bool
flagToMaybe False = Nothing
flagToMaybe True  = Just True
