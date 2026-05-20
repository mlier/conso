{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Ingestion.Parser
Description : Parser unifié des flux M023 Enedis (JSON → FluxRxx)

Définit 'FluxRxx', le type union de tous les flux possibles, et 'parseFluxRxx',
le point d'entrée de parsing qui dispatche selon le 'CodeFlux' fourni.

Particularité C68 : la racine JSON est un tableau d'objets (sans enveloppe
@header\/mesures@), traité séparément via 'parseFluxC68'.

Pour tous les autres flux, la racine JSON est un objet @{ header, mesures }@.
-}
module Conso.Fr.Elec.SiteDB.Ingestion.Parser
  ( FluxRxx(..)
  , parseFluxRxx
  ) where

import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Aeson             (FromJSON, eitherDecodeStrict, Value)
import           Conso.Fr.Elec.SiteDB.Types.Header (CodeFlux(..))
import           Conso.Fr.Elec.SiteDB.Types.R63    (FluxR63)
import           Conso.Fr.Elec.SiteDB.Types.R64    (FluxR64)
import           Conso.Fr.Elec.SiteDB.Types.R65    (FluxR65)
import           Conso.Fr.Elec.SiteDB.Types.R66    (FluxR66)
import           Conso.Fr.Elec.SiteDB.Types.R67    (FluxR67)
import           Conso.Fr.Elec.SiteDB.Types.C68    (InfoTechniqueContractuelle, parseFluxC68)
import           Conso.Fr.Elec.SiteDB.Types.Nass   (FluxNassJson(..), NassService)

-- | Type union de tous les flux M023 Enedis.
data FluxRxx
  = FluxCourbeCharge FluxR63                  -- ^ R63, R63A, R63B — courbes de charge
  | FluxIndex        FluxR64                  -- ^ R64, R64A, R64B — index compteur
  | FluxEnergie      FluxR65                  -- ^ R65 — énergies quotidiennes
  | FluxPmax         FluxR66                  -- ^ R66, R66B — puissances maximales
  | FluxFacturant    FluxR67                  -- ^ R67 — mesures facturantes
  | FluxITC          [InfoTechniqueContractuelle] -- ^ C68 — informations techniques
  | FluxNass         [NassService]            -- ^ NASS — arrêts de services souscrits
  deriving (Show)

-- | Parse un 'ByteString' JSON en 'FluxRxx' selon le 'CodeFlux' fourni.
--
-- Cas particulier C68 : la racine JSON est un tableau (sans enveloppe
-- @header\/mesures@). Pour tous les autres flux, la racine est un objet
-- @{ header, mesures }@.
parseFluxRxx :: CodeFlux -> ByteString -> Either Text FluxRxx
parseFluxRxx CF_C68 bs =
  case eitherDecodeStrict bs :: Either String Value of
    Left msg -> Left (T.pack msg)
    Right v  -> case parseFluxC68 v of
      Left  msg   -> Left (T.pack msg)
      Right items -> Right (FluxITC items)
parseFluxRxx CF_NASS bs =
  case eitherDecodeStrict bs :: Either String FluxNassJson of
    Left  msg -> Left (T.pack msg)
    Right (FluxNassJson services) -> Right (FluxNass services)
parseFluxRxx cf bs =
  case cf of
    CF_R63  -> FluxCourbeCharge <$> decodeFlux bs
    CF_R63A -> FluxCourbeCharge <$> decodeFlux bs
    CF_R63B -> FluxCourbeCharge <$> decodeFlux bs
    CF_R64  -> FluxIndex        <$> decodeFlux bs
    CF_R64A -> FluxIndex        <$> decodeFlux bs
    CF_R64B -> FluxIndex        <$> decodeFlux bs
    CF_R65  -> FluxEnergie      <$> decodeFlux bs
    CF_R66  -> FluxPmax         <$> decodeFlux bs
    CF_R66B -> FluxPmax         <$> decodeFlux bs
    CF_R67  -> FluxFacturant    <$> decodeFlux bs

decodeFlux :: FromJSON a => ByteString -> Either Text a
decodeFlux bs = case eitherDecodeStrict bs of
  Left  msg -> Left  (T.pack msg)
  Right val -> Right val
