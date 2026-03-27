{-|
Module      : Conso.Fr.Gaz.Adict
Description : API GRDF ADICT — Point d'entrée de la bibliothèque

Ré-exporte les modules principaux de la bibliothèque @conso-gaz-adict@.

Usage minimal :

> import Conso.Fr.Gaz.Adict
>
> main :: IO ()
> main = do
>     session <- initSession False        -- bac à sable
>     rep <- consulterConsosPubliees session "12345678901234" (ByPeriode "2024")
>     case rep of
>         Left  err -> print err
>         Right lst -> mapM_ print lst
-}
module Conso.Fr.Gaz.Adict
  ( module Conso.Fr.Gaz.Adict.Adict
  , module Conso.Fr.Gaz.Adict.Types
  , module Conso.Fr.Gaz.Adict.ConsosPubliees
  , module Conso.Fr.Gaz.Adict.ConsosInfos
  , module Conso.Fr.Gaz.Adict.DonneesContractuelles
  , module Conso.Fr.Gaz.Adict.DonneesTechniques
  , module Conso.Fr.Gaz.Adict.InjectionsPubliees
  , module Conso.Fr.Gaz.Adict.DroitsAcces
  , module Conso.Fr.Gaz.Adict.DroitAcces
  ) where

import Conso.Fr.Gaz.Adict.Adict
import Conso.Fr.Gaz.Adict.Types
import Conso.Fr.Gaz.Adict.ConsosPubliees
import Conso.Fr.Gaz.Adict.ConsosInfos
import Conso.Fr.Gaz.Adict.DonneesContractuelles
import Conso.Fr.Gaz.Adict.DonneesTechniques
import Conso.Fr.Gaz.Adict.InjectionsPubliees
import Conso.Fr.Gaz.Adict.DroitsAcces
import Conso.Fr.Gaz.Adict.DroitAcces
