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
-- GDA — déclaration passants (JDD 1-7)

pceGdaACF  :: Text ; pceGdaACF  = "09999999900617"  -- JDD 1 — AUTORISE_CONTRAT_FOURNITURE
pceGdaACF2 :: Text ; pceGdaACF2 = "09999999975102"  -- JDD 2
pceGdaACF3 :: Text ; pceGdaACF3 = "GI999055"        -- JDD 3
pceGdaACF4 :: Text ; pceGdaACF4 = "GI999947"        -- JDD 4
pceGdaDCF  :: Text ; pceGdaDCF  = "09999999928289"  -- JDD 5 — DETENTEUR_CONTRAT_FOURNITURE
pceGdaACI  :: Text ; pceGdaACI  = "GI999602"        -- JDD 6 — AUTORISE_CONTRAT_INJECTION
pceGdaDCI  :: Text ; pceGdaDCI  = "GI999150"        -- JDD 7 — DETENTEUR_CONTRAT_INJECTION

-- GDA — déclaration non passants (JDD 8-24)

pceGdaNR08 :: Text ; pceGdaNR08 = "GI999159"        -- droit d'accès existe déjà
pceGdaNR09 :: Text ; pceGdaNR09 = "09999999900609"  -- rôle non renseigné
pceGdaNR10 :: Text ; pceGdaNR10 = "GI999888"        -- erreur technique serveur
pceGdaNR11 :: Text ; pceGdaNR11 = "09999999988256"  -- contrat échu
pceGdaNR12 :: Text ; pceGdaNR12 = "09999999900612"  -- code postal incorrect (attendu ≠ 13000)
pceGdaNR13 :: Text ; pceGdaNR13 = "0999999999999"   -- PCE inconnu
pceGdaNR14 :: Text ; pceGdaNR14 = "09999999900614"  -- nom et raison_sociale tous deux vides
pceGdaNR15 :: Text ; pceGdaNR15 = "09999999900615"  -- nom et raison_sociale tous deux renseignés
pceGdaNR16 :: Text ; pceGdaNR16 = "45697829232770"  -- code postal incorrect (attendu ≠ 56000)
pceGdaNR17 :: Text ; pceGdaNR17 = "0999999990061"   -- format PCE incorrect
pceGdaNR18 :: Text ; pceGdaNR18 = "09999999900618"  -- code postal non renseigné
pceGdaNR19 :: Text ; pceGdaNR19 = "09999999900619"  -- format code postal incorrect
pceGdaNR20 :: Text ; pceGdaNR20 = "09999999900620"  -- email titulaire non renseigné
pceGdaNR21 :: Text ; pceGdaNR21 = "09999999900621"  -- périmètre contractuelles non renseigné
pceGdaNR22 :: Text ; pceGdaNR22 = "09999999900622"  -- date début périmètre conso manquante
pceGdaNR23 :: Text ; pceGdaNR23 = "GI999602"        -- date fin périmètre injection manquante
pceGdaNR24 :: Text ; pceGdaNR24 = "09999999900624"  -- format email incorrect

-- ---------------------------------------------------------------------------
-- GDA — révocation (JDD 30-32)

-- | JDD 30 : UUID passant pour la révocation.
uuidRevoquerPassant :: Text ; uuidRevoquerPassant = "f13a3109-39a7-47a0-8017-3192620dee4e"
-- | JDD 31 : droit inexistant (révoqué).
uuidRevoquerNR31    :: Text ; uuidRevoquerNR31    = "3edbd20d-34aa-4ef2-b5f1-dadc211acfb0"
-- | JDD 32 : PCE utilisé comme UUID → erreur technique.
uuidRevoquerNR32    :: Text ; uuidRevoquerNR32    = "09999999900617"

-- ---------------------------------------------------------------------------
-- GDA — preuves (JDD 33-36)

-- | JDD 33 : UUID passant pour la soumission d'une preuve (statut "Preuve en cours de vérification").
uuidPreuvePassant1 :: Text ; uuidPreuvePassant1 = "34ed04b3-1371-4418-93fe-0467760c7919"
-- | JDD 34 : UUID passant (statut "Preuve en attente").
uuidPreuvePassant2 :: Text ; uuidPreuvePassant2 = "90494e79-bfee-474e-888a-ea653f48cac6"
-- | JDD 35 : taille > 4 Mo.
uuidPreuveNR35     :: Text ; uuidPreuveNR35     = "34ed04b3-1371-4418-93fe-0467760c7920"
-- | JDD 36 : format non autorisé.
uuidPreuveNR36     :: Text ; uuidPreuveNR36     = "34ed04b3-1371-4418-93fe-0467760c7921"
