{-# LANGUAGE OverloadedStrings #-}
-- | Données de référence pour les tests d'intégration ADICT GRDF (bac à sable).
--   PCEs et paramètres extraits du JDD BAS v1.4.
module TestData where

import Data.Text (Text)


-- ---------------------------------------------------------------------------
-- CONSO PUBLIEES 
pceConsoPub1 :: Text ; pceConsoPub1 = "sdkgsl"


-- ---------------------------------------------------------------------------
-- CONSO INFORMATIVES 

pceConsoInfo1     :: Text ; pceConsoInfo1     = "GI999947"
pceConsoInfo2     :: Text ; pceConsoInfo2     = "09999999928289"
pceConsoInfo3     :: Text ; pceConsoInfo3     = "09999999928283"
pceConsoInfo4     :: Text ; pceConsoInfo4     = "0999999992828"
pceConsoInfo5     :: Text ; pceConsoInfo5     = "09999999900617"
pceConsoInfo6     :: Text ; pceConsoInfo6     = "GI999055"
pceConsoInfo7     :: Text ; pceConsoInfo7     = "09999999932770"
pceConsoInfo8     :: Text ; pceConsoInfo8     = "09999999975102"

dateDebutInfo1    :: Text ; dateDebutInfo1    = "2020-02-26"
dateDebutInfo2    :: Text ; dateDebutInfo2    = "2023-02-26"
dateFinInfo1      :: Text ; dateFinInfo1      = "2023-02-28"
dateFinInfo2      :: Text ; dateFinInfo2      = "2050-02-28"

dateDebutInfo3    :: Text ; dateDebutInfo3    = "2019-09-01"
dateFinInfo3      :: Text ; dateFinInfo3      = "2019-10-01"

periodeConsoInfo1 :: Text ; periodeConsoInfo1 = "2023-09-01"
periodeConsoInfo2 :: Text ; periodeConsoInfo2 = "2023-09"
periodeConsoInfo3 :: Text ; periodeConsoInfo3 = "2023"


-- ---------------------------------------------------------------------------
-- INJ PUBLIEES 

pceInj1      :: Text ; pceInj1      = "GI999150"
pceInj2      :: Text ; pceInj2      = "GI999602"
pceInj3      :: Text ; pceInj3      = "GI999947"

dateDebutInj1 :: Text ; dateDebutInj1 = "2018-07-01"
dateDebutInj2 :: Text ; dateDebutInj2 = "2023-07-01"
dateFinInj   :: Text ; dateFinInj   = "2023-07-17"
dateFinInj2   :: Text ; dateFinInj2   = "2050-07-17"

-- ---------------------------------------------------------------------------
-- DONNEES CONTRACTUELLES 

pceDonneesContrac :: Text ; pceDonneesContrac = "09999999900617"
pceDonneesContrac1 :: Text ; pceDonneesContrac1 = "09999999975102"

-- ---------------------------------------------------------------------------
-- DONNEES TECHNIQUES 

pceDonneesTech :: Text ; pceDonneesTech = "09999999975102"
pceDonneesTech1 :: Text ; pceDonneesTech1 = "GI999055"


-- ---------------------------------------------------------------------------
-- GDA — déclaration

pceGdaACF :: Text ; pceGdaACF = "09999999900617"  -- AUTORISE_CONTRAT_FOURNITURE


-- ---------------------------------------------------------------------------
-- GDA — révocation et preuves (UUIDs pré-existants dans la sandbox JDD BAS v1.4)

-- | JDD 30 : UUID passant pour la révocation.
uuidRevoquerPassant :: Text ; uuidRevoquerPassant = "f13a3109-39a7-47a0-8017-3192620dee4e"

-- | JDD 33 : UUID passant pour la soumission d'une preuve.
uuidPreuvePassant1 :: Text ; uuidPreuvePassant1 = "34ed04b3-1371-4418-93fe-0467760c7919"
