{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Options.Applicative.Help.Pretty ( vsep, pretty, Doc )
import qualified Data.Text                       as T
import           Data.Text                       ( Text )
import           Data.Time                       ( getCurrentTime, utctDay
                                                 , addGregorianYearsRollOver
                                                 , formatTime, defaultTimeLocale )
import           System.Exit                     ( die )
import           Text.Pretty.Simple
    ( pPrintOpt, CheckColorTty(..), defaultOutputOptionsDarkBg
    , StringOutputStyle(..), OutputOptions(..) )

import           Conso.Fr.Gaz.Adict.Adict
import           Conso.Fr.Gaz.Adict.Types
import           Conso.Fr.Gaz.Adict.ConsosPubliees
import           Conso.Fr.Gaz.Adict.ConsosInfos
import           Conso.Fr.Gaz.Adict.DonneesContractuelles
import           Conso.Fr.Gaz.Adict.DonneesTechniques
import           Conso.Fr.Gaz.Adict.InjectionsPubliees
import           Conso.Fr.Gaz.Adict.DroitsAcces
import           Conso.Fr.Gaz.Adict.DroitAcces
import           Conso.Fr.Gaz.Adict.Preuves

import           Display
import           Display.ConsoDisplay        ( ConsosPubliees(..), ConsosInfos(..) )
import           Display.DroitsAccesDisplay  ()
import           Display.DonneesDisplay      ()


-- ---------------------------------------------------------------------------
-- Structure des options

data Options = Options
    { optProd    :: Bool
    , optRaw     :: Bool
    , optDebug   :: Bool
    , optVerbose :: Bool
    , optCommand :: Command
    } deriving (Show)

data Command
    = Consos        ConsosOpts
    | ConsosInfo    ConsosOpts
    | Injections    ConsosOpts
    | Contrat       ContratOpts
    | Tech          TechOpts
    | Droits        FiltreOpts
    | Acces         AccesOpts
    | Revoquer      RevoquerOpts
    | PreuvesAttente
    | Preuve        PreuveOpts
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
    , acDuree         :: Maybe Int   -- Just N = durée N ans calculée auto
    } deriving (Show)

newtype RevoquerOpts = RevoquerOpts { rvId :: String } deriving (Show)

data PreuveOpts = PreuveOpts
    { pvId      :: String
    , pvFichier :: String
    } deriving (Show)


-- ---------------------------------------------------------------------------
-- Parseurs

opts :: Parser Options
opts = Options
    <$> switch ( long "prod"    <> help "Serveur de production (défaut : bac à sable)" )
    <*> switch ( long "raw"     <> help "Afficher la réponse brute (pPrint)" )
    <*> switch ( long "debug"   <> help "Afficher les requêtes HTTP sur stderr" )
    <*> switch ( long "verbose" <> help "Afficher les corps de réponse JSON sur stderr" )
    <*> commandParser


commandParser :: Parser Command
commandParser =
    subparser
      (  commandGroup "Données de consommation/injection"
      <> command "conso"
           ( info (Consos <$> consosParser <**> helper)
                  ( fullDesc
                  <> progDesc "Consulter les consommations publiées d'un PCE"
                  <> footerDoc (Just aidePeriode) ) )
      <> command "conso-info"
           ( info (ConsosInfo <$> consosParser <**> helper)
                  ( fullDesc
                  <> progDesc "Consulter les consommations informatives d'un PCE"
                  <> footerDoc (Just aidePeriode) ) )
      <> command "injection"
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
      <> command "liste"
           ( info (Droits <$> filtreParser <**> helper)
                  ( fullDesc
                  <> progDesc "Consulter mes droits d'accès (sans filtre : GET, avec filtre : POST)"
                  <> footerDoc (Just aideDroits) ) )
      <> command "declarer"
           ( info (Acces <$> accesParser <**> helper)
                  ( fullDesc
                  <> progDesc "Déclarer un droit d'accès aux données d'un PCE"
                  <> footerDoc (Just aideAcces) ) )
      <> command "revoquer"
           ( info (Revoquer <$> revoquerParser <**> helper)
                  ( fullDesc 
                  <> progDesc "Révoquer un droit d'accès (danger !)"
                  <> footerDoc (Just aideRevoquer) ) )
      <> command "preuves-attente"
           ( info (pure PreuvesAttente <**> helper)
                  (progDesc "Lister les droits d'accès en attente de preuve de consentement") )
      <> command "preuve"
           ( info (Preuve <$> preuveParser <**> helper)
                  (progDesc "Transmettre une preuve de consentement pour un droit d'accès") )
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
    <$> optional (strOption ( long "role"   <> metavar "ROLE"   <> help "Rôle tiers : acf, dcf, aci, dci" ))
    <*> optional (strOption ( long "pce"    <> metavar "PCE"    <> help "Identifiant PCE" ))
    <*> optional (strOption ( long "statut" <> metavar "STATUT" <> help "Statut contrôle preuve : Attente, Vérification, Validée, SansObjet" ))
    <*> optional (strOption ( long "etat"   <> metavar "ETAT"   <> help "État du droit d'accès : Active, Obsolète, Refusé" ))  

accesParser :: Parser AccesOpts
accesParser = AccesOpts
    <$> strOption ( long "pce"   <> metavar "PCE" <> help "Identifiant PCE" )
    <*> strOption ( long "role"  <> metavar "ROLE"
                  <> value "acf"
                  <> showDefault
                  <> help "Rôle tiers : acf, dcf, aci, dci" )
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
    <*> optional
          (   flag' 1 (long "1an"  <> help "Durée 1 an  : dates calculées automatiquement")
          <|> flag' 2 (long "2ans" <> help "Durée 2 ans : dates calculées automatiquement")
          <|> flag' 3 (long "3ans" <> help "Durée 3 ans : dates calculées automatiquement")
          )


revoquerParser :: Parser RevoquerOpts
revoquerParser = RevoquerOpts
    <$> strOption ( long "id" <> metavar "UUID" <> help "UUID du droit d'accès à révoquer" )

preuveParser :: Parser PreuveOpts
preuveParser = PreuveOpts
    <$> strOption ( long "id"      <> metavar "UUID"    <> help "UUID du droit d'accès" )
    <*> strOption ( long "fichier" <> metavar "FICHIER" <> help "Chemin vers le fichier de preuve (PDF/image, max 4 Mo)" )


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

aideDroits :: Doc
aideDroits = vsep
    [ pretty ("" :: String)
    , pretty ("Sans filtre : GET /droits_acces (tous mes droits d'accès)." :: String)
    , pretty ("Avec au moins un filtre : POST /droits_acces (recherche filtrée)." :: String)
    , pretty ("" :: String)
    , pretty ("Rôles disponibles :" :: String)
    , pretty ("  acf : régime d'Autorisation pour accéder au Contrat de Fourniture" :: String)
    , pretty ("  dcf : régime de Détention du Contrat de Fourniture" :: String)
    , pretty ("  aci : régime d'Autorisation pour accéder au Contrat d'Injection" :: String)
    , pretty ("  dci : régime de Détention du Contrat d'Injection" :: String)
    , pretty ("" :: String)
    , pretty ("Valeurs de --etat  : actif, avalider, revoque, areverifier, obsolete, refuse" :: String)
    , pretty ("Valeurs de --statut: attente, verification, verifok, verifko" :: String)
    ]

aideAcces :: Doc
aideAcces = vsep
    [ pretty ("" :: String)
    , pretty ("Rôles disponibles :" :: String)
    , pretty ("  acf : régime d'Autorisation pour accéder au Contrat de Fourniture (défaut)" :: String)
    , pretty ("  dcf : régime de Détention du Contrat de Fourniture" :: String)
    , pretty ("  aci : régime d'Autorisation pour accéder au Contrat d'Injection" :: String)
    , pretty ("  dci : régime de Détention du Contrat d'Injection" :: String)
    , pretty ("" :: String)
    , pretty ("Durée automatique (--1an / --2ans / --3ans) :" :: String)
    , pretty ("  début accès  = aujourd'hui" :: String)
    , pretty ("  fin accès    = aujourd'hui + N ans" :: String)
    , pretty ("  début conso  = aujourd'hui − 5 ans" :: String)
    , pretty ("  fin conso    = aujourd'hui + N ans" :: String)
    , pretty ("" :: String)
    , pretty ("Exemple avec durée automatique :" :: String)
    , pretty ("  conso-gaz-adict declarer --pce 12345678901234 --cp 75001 \\" :: String)
    , pretty ("    --nom 'Dupont Jean' --email client@example.com --2ans \\" :: String)
    , pretty ("    --contractuelles --techniques --informatives --publiees" :: String)
    , pretty ("" :: String)
    , pretty ("Exemple avec dates manuelles :" :: String)
    , pretty ("  conso-gaz-adict declarer --pce 12345678901234 --cp 75001 --role acf \\" :: String)
    , pretty ("    --raison 'Ma Société SAS' --email client@example.com \\" :: String)
    , pretty ("    --debut-acces 2024-01-01 --fin-acces 2025-01-01 \\" :: String)
    , pretty ("    --debut-conso 2023-01-01 --fin-conso 2025-01-01 \\" :: String)
    , pretty ("    --contractuelles --techniques --informatives --publiees" :: String)
    ]

aideRevoquer :: Doc
aideRevoquer = vsep
    [ pretty ("La révocation a pour conséquence de bloquer définitivement" :: String)
    , pretty ("les nouvelles déclarations de droit d'accès pour le PCE" :: String)
    , pretty ("via le parcours Tiers Direct." :: String)
    ]
-- ---------------------------------------------------------------------------
-- Exécution
main :: IO ()
main = do
    o <- execParser $ info (opts <**> helper)
            ( fullDesc
            <> progDesc "Client GRDF API ADICT — consultation des données PCE"
            <> header "conso-gaz-adict — GRDF ADICT B2B v2" )
    session <- initSession (optProd o) (optDebug o) (optVerbose o)
    run session (optRaw o) (optCommand o)


run :: AdictSession -> Bool -> Command -> IO ()
run session raw cmd = case cmd of

    Consos co -> do
        rep <- consulterConsosPubliees session (packT (coPce co)) (mkPeriode co)
        if raw then pPrintUtf8 rep else renderApp (ConsosPubliees <$> rep)

    ConsosInfo co -> do
        rep <- consulterConsosInfos session (packT (coPce co)) (mkPeriode co)
        if raw then pPrintUtf8 rep else renderApp (ConsosInfos <$> rep)

    Injections co -> do
        rep <- consulterInjectionsPubliees session (packT (coPce co)) (mkPeriode co)
        if raw then pPrintUtf8 rep else renderApp rep

    Contrat ct -> do
        rep <- consulterDonneesContractuelles session (packT (ctPce ct)) []
        if raw then pPrintUtf8 rep else renderApp rep

    Tech t -> do
        rep <- consulterDonneesTechniques session (packT (techPce t))
        if raw then pPrintUtf8 rep else renderApp rep

    Droits fo -> do
        role   <- parseFiltre "--role"   roleTiersFromCli    (foRole   fo)
        statut <- parseFiltre "--statut" statutFromCli       (foStatut fo)
        etat   <- parseFiltre "--etat"   etatFromCli         (foEtat   fo)
        let filtre = FiltreAcces
                { fa_role_tiers             = role
                , fa_id_pce                 = maybe [] (pure . packT) (foPce fo)
                , fa_statut_controle_preuve = statut
                , fa_etat_droit_acces       = etat
                }
        rep <- if filtreVide filtre
                   then consulterDroitsAcces session
                   else rechercherDroitsAcces session filtre
        if raw then pPrintUtf8 rep else renderApp rep

    Acces ao -> do
        role <- expandRole (acRole ao)
        (dDebutAcces, dFinAcces, dDebutConso, dFinConso) <- case acDuree ao of
            Nothing -> return ( acDebutAcces ao, acFinAcces ao
                              , acDebutConso ao, acFinConso ao )
            Just n  -> do
                today <- utctDay <$> getCurrentTime
                let finDate    = addGregorianYearsRollOver (fromIntegral n) today
                    debutConso = addGregorianYearsRollOver (-5) today
                    fmt d      = Just (formatTime defaultTimeLocale "%Y-%m-%d" d)
                return (fmt today, fmt finDate, fmt debutConso, fmt finDate)
        let demande = DemandeAccesIn
                { din_role_tiers                        = role
                , din_raison_sociale                    = fmap packT (acRaisonSociale ao)
                , din_nom_titulaire                     = fmap packT (acNom ao)
                , din_code_postal                       = packT (acCp ao)
                , din_courriel_titulaire                = fmap packT (acEmail ao)
                , din_numero_telephone_mobile_titulaire = fmap packT (acTel ao)
                , din_date_debut_droit_acces            = fmap packT dDebutAcces
                , din_date_fin_droit_acces              = fmap packT dFinAcces
                , din_perim_donnees_conso_debut         = fmap packT dDebutConso
                , din_perim_donnees_conso_fin           = fmap packT dFinConso
                , din_perim_donnees_inj_debut           = Nothing
                , din_perim_donnees_inj_fin             = Nothing
                , din_perim_donnees_contractuelles      = flagToMaybe (acContrat ao)
                , din_perim_donnees_techniques          = flagToMaybe (acTech ao)
                , din_perim_donnees_informatives        = flagToMaybe (acInfos ao)
                , din_perim_donnees_publiees            = flagToMaybe (acPubliees ao)
                }
        rep <- declarerDroitAcces session (packT (acPce ao)) demande
        if raw then pPrintUtf8 rep else renderApp rep

    Revoquer rv -> do
        rep <- revoquerDroitAcces session (packT (rvId rv))
        if raw then pPrintUtf8 rep else renderApp rep

    PreuvesAttente -> do
        rep <- consulterPreuvesAFournir session
        if raw then pPrintUtf8 rep else renderApp rep

    Preuve pv -> do
        rep <- soumettrePrevue session (packT (pvId pv)) (pvFichier pv)
        case rep of
            Left  err -> pPrintUtf8 (Left err :: Either AdictError ())
            Right ()  -> putStrLn "Preuve transmise avec succès."


-- ---------------------------------------------------------------------------
-- Petits helpers

packT :: String -> Text
packT = T.pack

mkPeriode :: ConsosOpts -> PeriodeParam
mkPeriode co = case (coPeriode co, coDebut co, coFin co) of
    (Just p, _,      _)      -> ByPeriode   (packT p)
    (_,      Just d, Just f) -> ByDateRange (packT d) (packT f)
    _                        -> ByPeriode "2024"

-- | Parse une valeur de filtre CLI, échoue explicitement si inconnue.
parseFiltre :: String -> (T.Text -> Maybe a) -> Maybe String -> IO [a]
parseFiltre _    _    Nothing  = return []
parseFiltre myflag conv (Just s) = case conv (packT s) of
    Just v  -> return [v]
    Nothing -> die $ "Valeur invalide pour " ++ myflag ++ " : " ++ show s

-- | Expand un alias court de rôle (acf/dcf/aci/dci) ou accepte la valeur
--   complète. Retourne le texte canonique ou échoue avec un message d'erreur.
expandRole :: String -> IO T.Text
expandRole s = case s of
    "acf"                          -> ok "AUTORISE_CONTRAT_FOURNITURE"
    "dcf"                          -> ok "DETENTEUR_CONTRAT_FOURNITURE"
    "aci"                          -> ok "AUTORISE_CONTRAT_INJECTION"
    "dci"                          -> ok "DETENTEUR_CONTRAT_INJECTION"
    "AUTORISE_CONTRAT_FOURNITURE"  -> ok "AUTORISE_CONTRAT_FOURNITURE"
    "DETENTEUR_CONTRAT_FOURNITURE" -> ok "DETENTEUR_CONTRAT_FOURNITURE"
    "AUTORISE_CONTRAT_INJECTION"   -> ok "AUTORISE_CONTRAT_INJECTION"
    "DETENTEUR_CONTRAT_INJECTION"  -> ok "DETENTEUR_CONTRAT_INJECTION"
    _                              -> die $ "Rôle invalide : " ++ show s
                                        ++ " (valeurs : acf, dcf, aci, dci)"
  where ok = return . T.pack

-- | Alias courts pour role_tiers (accepte aussi les noms complets).
roleTiersFromCli :: T.Text -> Maybe RoleTiers
roleTiersFromCli "acf" = Just AutoriseContratFourniture
roleTiersFromCli "dcf" = Just DetenteurContratFourniture
roleTiersFromCli "aci" = Just AutoriseContratInjection
roleTiersFromCli "dci" = Just DetenteurContratInjection
roleTiersFromCli t     = roleTiersFromText t

-- | Shortcuts CLI pour etat_droit_acces.
etatFromCli :: T.Text -> Maybe EtatDroitAcces
etatFromCli "actif"       = Just EtatActive
etatFromCli "avalider"    = Just EtatAValider
etatFromCli "revoque"     = Just EtatRevoquee
etatFromCli "areverifier" = Just EtatAReverifier
etatFromCli "obsolete"    = Just EtatObsolete
etatFromCli "refuse"      = Just EtatRefusee
etatFromCli _             = Nothing

-- | Shortcuts CLI pour statut_controle_preuve.
statutFromCli :: T.Text -> Maybe StatutControlePreuve
statutFromCli "attente"      = Just PreuveEnAttente
statutFromCli "verification" = Just PreuveEnCoursDeVerification
statutFromCli "verifok"   = Just PreuveVerifieeOK
statutFromCli "verifko"   = Just PreuveVerifieeKO
statutFromCli _              = Nothing

pPrintUtf8 :: Show a => a -> IO ()
pPrintUtf8 = pPrintOpt CheckColorTty
    defaultOutputOptionsDarkBg { outputOptionsStringStyle = DoNotEscapeNonPrintable }

flagToMaybe :: Bool -> Maybe T.Text
flagToMaybe False = Nothing
flagToMaybe True  = Just "true"

filtreVide :: FiltreAcces -> Bool
filtreVide f = null (fa_role_tiers f)
            && null (fa_id_pce f)
            && null (fa_statut_controle_preuve f)
            && null (fa_etat_droit_acces f)
