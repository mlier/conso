-- | Données de référence pour les tests d'homologation SGE v26.1.
--   Centraliser ici tous les PRMs et dates afin de les mettre à jour
--   facilement entre deux versions du catalogue.
module TestData where


-- ---------------------------------------------------------------------------
-- ConsulterDonneesTechniquesContractuellesV10 (ADP)

adpPrmC5 :: String
adpPrmC5 = "25946599093143"

adpPrmC1C4 :: String
adpPrmC1C4 = "98800004847471"


-- ---------------------------------------------------------------------------
-- ConsulterMesuresV11 (AHC)

ahcPrmC5 :: String
ahcPrmC5 = "25162373298976"

ahcPrmC1C4 :: String
ahcPrmC1C4 = "98800000396971"


-- ---------------------------------------------------------------------------
-- ConsulterMesuresDetailleesV3 (CMD3)

cmd3PrmC5 :: String
cmd3PrmC5 = "25162373298976"

cmd3PrmC1C4a :: String
cmd3PrmC1C4a = "98800001144455"

cmd3PrmC1C4b :: String
cmd3PrmC1C4b = "98800003605936"

-- | Période courte (~1 semaine) – cas recevables.
cmd3DateDebut :: String
cmd3DateDebut = "2025-04-01"

cmd3DateFin :: String
cmd3DateFin = "2025-04-07"

-- | Période longue (> 2 ans) – cas non-recevable CMD3-NR1.
cmd3DateDebutLong :: String
cmd3DateDebutLong = "2022-01-01"

cmd3DateFinLong :: String
cmd3DateFinLong = "2025-04-01"


-- ---------------------------------------------------------------------------
-- RechercherPointV20 (RP)

-- | RP-R1 : recherche par code postal + commune INSEE seuls.
rpCodePostalR1 :: String
rpCodePostalR1 = "34650"

rpInseeR1 :: String
rpInseeR1 = "34231"

-- | RP-R2 : recherche par code postal + commune + nom client.
rpCodePostalR2 :: String
rpCodePostalR2 = "84160"

rpInseeR2 :: String
rpInseeR2 = "84042"

rpNomClientR2 :: String
rpNomClientR2 = "TEST"

-- | RP-R3 : recherche par code postal + commune + voie.
rpCodePostalR3 :: String
rpCodePostalR3 = "84160"

rpInseeR3 :: String
rpInseeR3 = "84042"

rpVoieR3 :: String
rpVoieR3 = "1 RUE DE LA MER"


-- ---------------------------------------------------------------------------
-- CommanderAccesDonneesMesuresV10 (ACCES)

accesPrmC5 :: String
accesPrmC5 = "24380318190106"

accesPrmC2C4 :: String
accesPrmC2C4 = "98800003605600"


-- ---------------------------------------------------------------------------
-- CommanderServicesAccesDonneesV10 (SAD) + RechercherServicesAccesDonneesV10 (RSA)
-- + CommanderArretServicesAccesDonneesV10 (ASAD)
-- + CommanderRenouvellementServicesAccesDonneesV10 (RSAD)

sadPrmC5R1 :: String  -- SAD-R1 ENERGIE | RSA-R1 | RSAD-R1, NR1
sadPrmC5R1 = "25855571545617"

sadPrmC5R2 :: String  -- SAD-R2 CDC
sadPrmC5R2 = "24377424002398"

sadPrmC5R3 :: String  -- SAD-R3 IDX | ASAD-R1
sadPrmC5R3 = "24380318190106"

sadPrmC5R4 :: String  -- SAD-R4 IDX (multi-service)
sadPrmC5R4 = "25852098337945"

sadPrmC2C4 :: String  -- Segment C2-C4 commun SAD-R1..R4
sadPrmC2C4 = "98800003605600"

-- | Periodicité de transmission pour les options MOSAD (ex : "J" = journalier).
--   À ajuster selon le catalogue en vigueur.
mosadPeriodicite :: String
mosadPeriodicite = "J"


-- ---------------------------------------------------------------------------
-- CommanderTransmissionDonneesInfraJV10 (F375A)

f375aPrmC2C4 :: String
f375aPrmC2C4 = "98800000000246"


-- ---------------------------------------------------------------------------
-- DemandePublicationMesuresFinesM23V10 (MFI-GK)

mfiPrmsC5 :: [String]
mfiPrmsC5 =
    [ "25150217034354"
    , "25825036170379"
    , "25999131613803"
    , "50086054270348"
    , "25262662681289"
    ]

mfiPrmsC2C4 :: [String]
mfiPrmsC2C4 =
    [ "98800004935121"
    , "98800001544168"
    , "98800004938924"
    , "98800006694381"
    , "98800001220186"
    ]

-- | Période pour les cas recevables MFI-GK.
mfiDateDebut :: String
mfiDateDebut = "2025-01-01"

mfiDateFin :: String
mfiDateFin = "2025-02-01"

-- | Dates longues pour MFI-GK NR (profondeur > limite).
mfiDateDebutNR1 :: String   -- CDC > 24 mois avant mfiDateFin="2025-02-01"
mfiDateDebutNR1 = "2022-01-01"

mfiDateDebutNR2 :: String   -- IDX > 36 mois avant mfiDateFin="2025-02-01"
mfiDateDebutNR2 = "2021-01-01"


-- ---------------------------------------------------------------------------
-- DemandePublicationMesuresFacturantesM23V10 (MFA-GK)
-- Mêmes PRMs et période que MFI-GK.

mfaPrmsC5 :: [String]
mfaPrmsC5 = mfiPrmsC5

mfaPrmsC2C4 :: [String]
mfaPrmsC2C4 = mfiPrmsC2C4

mfaDateDebut :: String
mfaDateDebut = mfiDateDebut

mfaDateFin :: String
mfaDateFin = mfiDateFin


-- ---------------------------------------------------------------------------
-- DemandePublicationInformationsTechniquesContractuellesM23V10 (ITC-GK)
-- Mêmes PRMs que MFI-GK (pas de période pour ITC).

itcPrmsC5 :: [String]
itcPrmsC5 = mfiPrmsC5

itcPrmsC2C4 :: [String]
itcPrmsC2C4 = mfiPrmsC2C4
