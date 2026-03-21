{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.C68 where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import qualified Data.Aeson.Key      as Key
import           Conso.Fr.Elec.SgeDB.Types.Common (PrmId(..))

-- | C68 est stocké comme JSON brut car sa structure est très large (200+ champs).
-- On extrait uniquement les champs clés pour l'indexation et la détection de changements.
data InfoTechniqueContractuelle = InfoTechniqueContractuelle
  { c68IdPrm              :: PrmId
  , c68Segment            :: Maybe Text   -- situationsContractuelles[0].segment
  , c68EtatContractuel    :: Maybe Text   -- ..informationsContractuelles.etatContractuel
  , c68EtatAlimentation   :: Maybe Text   -- situationAlimentation.etatAlimentation
  , c68PuissanceSouscrite :: Maybe Text   -- ..structureTarifaire.puissanceSouscrite.valeur
  , c68DomaineTension     :: Maybe Text   -- situationAlimentation.alimentationPrincipale.domaineTension
  , c68RawJson            :: Value        -- JSON brut complet
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

-- | Extrait un champ de la première situationContractuelle (si elle existe)
firstSitContractuelle :: Object -> Text -> Parser (Maybe Text)
firstSitContractuelle o field = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (\s -> s .:? Key.fromText field) x
    _          -> pure Nothing

-- | Extrait un champ imbriqué dans la première situationContractuelle
firstSitContractuelleNested :: Object -> [Text] -> Parser (Maybe Text)
firstSitContractuelleNested o path = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (\s -> lookupNested s path) x
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

-- | Parse un tableau C68 (racine = array)
parseFluxC68 :: Value -> Either String [InfoTechniqueContractuelle]
parseFluxC68 v = case fromJSON v of
  Success items -> Right items
  Error msg     -> Left $ "Erreur parsing C68: " ++ msg
