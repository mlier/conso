{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Types.C68
Description : Types pour les Informations Techniques et Contractuelles Enedis (flux C68)

Représente le flux C68 (ITC — Informations Techniques et Contractuelles).
La structure JSON est un tableau d'objets, sans enveloppe @header\/mesures@.

Chaque objet couvre un PRM et peut contenir jusqu'à ~211 colonnes organisées en :
@DonneesGenerales@, @syntheseContractuelle@, @Rattachement(0..n)@,
@situationContractuelle(1..2)@, @optionContractuelle(0..1)@,
@InstallationProduction(0..n)@, @situationAlimentation(1)@, @situationComptage(1)@.

Seuls les champs clés sont extraits dans 'InfoTechniqueContractuelle' ;
l'intégralité du JSON est conservée dans 'c68RawJson' et accessible
via @exportPrmInfoJSON@.
-}
module Conso.Fr.Elec.SgeDB.Types.C68 where

import           Data.Text           (Text)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import qualified Data.Aeson.Key      as Key
import           Conso.Fr.Elec.SgeDB.Types.Common (PrmId(..))

-- | Informations Techniques et Contractuelles extraites d'un fichier C68.
--
-- Seuls quelques champs structurants sont extraits pour l'indexation ;
-- le JSON complet est conservé dans 'c68RawJson' (~211 colonnes).
data InfoTechniqueContractuelle = InfoTechniqueContractuelle
  { c68IdPrm              :: PrmId        -- ^ Identifiant du PRM (14 chiffres)
  , c68Segment            :: Maybe Text   -- ^ Segment de clientèle (@C1@-@C5@\/@P1@-@P4@) — @situationsContractuelles[0].segment@
  , c68EtatContractuel    :: Maybe Text   -- ^ État contractuel — @..informationsContractuelles.etatContractuel@
  , c68EtatAlimentation   :: Maybe Text   -- ^ État de l'alimentation — @situationAlimentation.etatAlimentation@
  , c68PuissanceSouscrite :: Maybe Text   -- ^ Puissance souscrite (kVA) — @..structureTarifaire.puissanceSouscrite.valeur@
  , c68DomaineTension     :: Maybe Text   -- ^ Domaine de tension (@BT@\/@HTA@\/@HTB@) — @situationAlimentation.alimentationPrincipale.domaineTension@
  , c68RawJson            :: Value        -- ^ JSON brut complet (re-parsé via @exportPrmInfoJSON@)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instance FromJSON

instance FromJSON InfoTechniqueContractuelle where
  parseJSON v = withObject "C68Item" parseC68Item v
    where
      parseC68Item o = do
        idPrm <- o .: "idPrm"
        seg   <- firstSitContractuelle o "segment"
        etat  <- firstSitContractuelleNested o
                   ["informationsContractuelles", "etatContractuel"]
        etatAl <- situationAlim o "etatAlimentation"
        puiss  <- firstSitContractuelleNested o
                   ["structureTarifaire", "puissanceSouscrite", "valeur"]
        domTen <- situationAlimNested o
                   ["alimentationPrincipale", "domaineTension"]
        pure $ InfoTechniqueContractuelle
          (PrmId idPrm) seg etat etatAl puiss domTen v

-- | Extrait un champ de la première @situationContractuelle@ (si elle existe).
firstSitContractuelle :: Object -> Text -> Parser (Maybe Text)
firstSitContractuelle o field = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (\s -> s .:? Key.fromText field) x
    _          -> pure Nothing

-- | Extrait un champ imbriqué dans la première @situationContractuelle@.
-- Le chemin est une liste de clés JSON à traverser successivement.
firstSitContractuelleNested :: Object -> [Text] -> Parser (Maybe Text)
firstSitContractuelleNested o path = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (`lookupNested` path) x
    _          -> pure Nothing

-- | Extrait un champ de situationAlimentation
situationAlim :: Object -> Text -> Parser (Maybe Text)
situationAlim o field = do
  mSit <- o .:? "situationAlimentation" :: Parser (Maybe Object)
  case mSit of
    Just sit -> sit .:? Key.fromText field
    Nothing  -> pure Nothing

-- | Extrait un champ imbriqué dans situationAlimentation
situationAlimNested :: Object -> [Text] -> Parser (Maybe Text)
situationAlimNested o path = do
  mSit <- o .:? "situationAlimentation" :: Parser (Maybe Object)
  case mSit of
    Just sit -> lookupNested sit path
    Nothing  -> pure Nothing

-- | Navigation dans un objet JSON selon une liste de clés
lookupNested :: Object -> [Text] -> Parser (Maybe Text)
lookupNested _ []     = pure Nothing
lookupNested o [k]    = o .:? Key.fromText k
lookupNested o (k:ks) = do
  mSub <- o .:? Key.fromText k :: Parser (Maybe Object)
  case mSub of
    Just sub -> lookupNested sub ks
    Nothing  -> pure Nothing

-- | Parse un fichier C68 dont la racine JSON est un tableau (sans enveloppe @header@).
-- Retourne une liste d''InfoTechniqueContractuelle', une par PRM présent dans le fichier.
parseFluxC68 :: Value -> Either String [InfoTechniqueContractuelle]
parseFluxC68 v = case fromJSON v of
  Success items -> Right items
  Error msg     -> Left $ "Erreur parsing C68: " ++ msg
