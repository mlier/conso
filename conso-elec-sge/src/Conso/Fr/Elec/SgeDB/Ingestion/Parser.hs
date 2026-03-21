{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Ingestion.Parser
  ( FluxRxx(..)
  , parseFluxRxx
  ) where

import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Aeson             (FromJSON, eitherDecodeStrict, Value)
import           Conso.Fr.Elec.SgeDB.Types.Header (CodeFlux(..))
import           Conso.Fr.Elec.SgeDB.Types.R63    (FluxR63)
import           Conso.Fr.Elec.SgeDB.Types.R64    (FluxR64)
import           Conso.Fr.Elec.SgeDB.Types.R65    (FluxR65)
import           Conso.Fr.Elec.SgeDB.Types.R66    (FluxR66)
import           Conso.Fr.Elec.SgeDB.Types.R67    (FluxR67)
import           Conso.Fr.Elec.SgeDB.Types.C68    (InfoTechniqueContractuelle, parseFluxC68)

-- | Type union de tous les flux possibles
data FluxRxx
  = FluxCourbeCharge FluxR63
  | FluxIndex        FluxR64
  | FluxEnergie      FluxR65
  | FluxPmax         FluxR66
  | FluxFacturant    FluxR67
  | FluxITC          [InfoTechniqueContractuelle]
  deriving (Show)

-- | Parse un ByteString JSON selon le code flux fourni.
-- Pour C68, la racine JSON est un tableau (pas d'enveloppe header/mesures).
-- Pour les autres flux, la racine est un objet { header, mesures }.
parseFluxRxx :: CodeFlux -> ByteString -> Either Text FluxRxx
parseFluxRxx CF_C68 bs =
  case eitherDecodeStrict bs :: Either String Value of
    Left msg -> Left (T.pack msg)
    Right v  -> case parseFluxC68 v of
      Left  msg   -> Left (T.pack msg)
      Right items -> Right (FluxITC items)
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
    CF_C68  -> Left "CF_C68 traité séparément"  -- unreachable

decodeFlux :: FromJSON a => ByteString -> Either Text a
decodeFlux bs = case eitherDecodeStrict bs of
  Left  msg -> Left  (T.pack msg)
  Right val -> Right val
