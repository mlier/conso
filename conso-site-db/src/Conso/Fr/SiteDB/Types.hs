{-|
Module      : Conso.Fr.SiteDB.Types
Description : Types communs pour l'identification des sites énergie

Un site physique peut avoir un PRM (électricité Enedis) et/ou un PCE (gaz GRDF).
L'identifiant canonique d'un site est un UUID v4, indépendant de tout identifiant
métier, stocké dans le registre central.
-}
module Conso.Fr.SiteDB.Types
  ( SiteId(..)
  , Prm(..)
  , Pce(..)
  , SiteLabel
  , SiteRef(..)
  ) where

import           Data.UUID   (UUID)
import           Data.Text   (Text)

-- | Identifiant unique d'un site, sous forme d'UUID v4.
-- Correspond au nom du fichier SQLite du site : @{uuid}.db@.
newtype SiteId = SiteId UUID
  deriving (Eq, Ord, Show)

-- | Point de Référence de Mesure Enedis (14 chiffres).
newtype Prm = Prm Text
  deriving (Eq, Ord, Show)

-- | Point de Comptage et d'Estimation GRDF.
newtype Pce = Pce Text
  deriving (Eq, Ord, Show)

-- | Nom libre associé à un site (ex : "Siège Lyon").
type SiteLabel = Text

-- | Référence complète d'un site telle que stockée dans le registre.
data SiteRef = SiteRef
  { srSiteId              :: SiteId
  , srPrm                 :: Maybe Prm
  , srPce                 :: Maybe Pce
  , srLabel               :: Maybe SiteLabel
  , srGazAvecInjections   :: Bool
  } deriving (Eq, Show)
