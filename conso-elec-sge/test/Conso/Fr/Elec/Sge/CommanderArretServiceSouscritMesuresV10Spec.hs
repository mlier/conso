module Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Spec where

import SpecHelper
import TestData (assPrmC5, f375aPrmC2C4)
import Data.Maybe (listToMaybe)

import qualified Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10 as ACCES
import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10
    ( AccordPersonneType(AccordPersonnePhysiqueNom), Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( CommanderAccesDonneesMesuresResponseType )

import qualified Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10 as RSS
import Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type
    ( RechercherServicesSouscritsMesuresResponseType
    , rechercherServicesSouscritsMesuresResponseType_servicesSouscritsMesures
    , servicesSouscritsMesuresType_serviceSouscritMesures
    , serviceSouscritMesuresType_serviceSouscritId
    , serviceSouscritMesuresType_etatCode )

import Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type
    ( CommanderArretServiceSouscritMesuresResponseType )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
    ( Chaine15Type(Chaine15Type) )
import Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString) )
import Data.Either (isRight)


-- | Cherche le premier service souscrit ACTIF et retourne son ID.
findActiveServiceId :: String -> IO (Maybe String)
findActiveServiceId prm = do
    rssType <- RSS.initTypeTest prm
    rep     <- wsRequestTest rssType
                   :: IO (Either (String, String) RechercherServicesSouscritsMesuresResponseType)
    return $ case rep of
        Left  _    -> Nothing
        Right resp ->
            let services = maybe []
                               servicesSouscritsMesuresType_serviceSouscritMesures
                               (rechercherServicesSouscritsMesuresResponseType_servicesSouscritsMesures resp)
                actifs   = filter isActif services
            in listToMaybe (map extractSid actifs)
  where
    isActif s =
        let Chaine15Type (XsdString code) = serviceSouscritMesuresType_etatCode s
        in code == "ACTIF"
    extractSid s =
        let Chaine15Type (XsdString sid) = serviceSouscritMesuresType_serviceSouscritId s
        in sid


shouldArretHomo :: String -> Expectation
shouldArretHomo prm = pendingOnNetworkError $ do
    cleanupServices prm
    accesType <- ACCES.initTypeTest prm (Just (3 * 365))
                     (Just (AccordPersonnePhysiqueNom "Toto")) "ENERGIE" SensSOUTIRAGE
    _         <- wsRequestTest accesType
                     :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    msid <- findActiveServiceId prm
    case msid of
        Nothing  -> pendingWith "Aucun service souscrit actif trouvé"
        Just sid -> do
            myType <- initTypeTest prm sid
            rep    <- wsRequestTest myType
                          :: IO (Either (String, String) CommanderArretServiceSouscritMesuresResponseType)
            rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "ASS-R1 C5 - Arrêt d'un service souscrit de mesures (CDC/IDX)" $
                shouldArretHomo assPrmC5
            it "ASS-R1 C2-C4 - Arrêt d'un service souscrit de mesures (CDC/IDX)" $
                shouldArretHomo f375aPrmC2C4


main :: IO ()
main = hspec spec
