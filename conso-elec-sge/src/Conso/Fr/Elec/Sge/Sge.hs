{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-|
Module      : Conso.Fr.Elec.Sge.Sge
Description : Infrastructure SOAP commune à tous les webservices SGE Enedis B2B

Fournit :

  * Les types de configuration 'SgeEnv', 'Sge' et 'Test' lus depuis
    @~\/.conso\/conso-env.yaml@
  * Les typeclasses 'RequestType' et 'ResponseType' qui relient chaque
    webservice à son URL, sa SOAPAction et son parseur XML
  * Les fonctions de haut niveau 'wsRequest' \/ 'wsRequestTest' pour envoyer
    une requête et obtenir une réponse typée, et 'xmlRequest' \/ 'xmlRequestTest'
    pour obtenir le XML brut

Le transport utilise HTTPS avec authentification mutuelle TLS (certificat client)
et authentification HTTP Basic, conformément aux exigences du portail SGE.
-}
module Conso.Fr.Elec.Sge.Sge  where

import           Control.Monad ( (>=>) )
import qualified Data.Text as T
import           Data.Text.Encoding as T ( encodeUtf8 )
import           Data.Text ( Text )
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE


import           Network.SOAP ( invokeWS, ResponseParser(RawParser) )
import           Network.SOAP.Transport.HTTP ( initTransportWithM, RequestProc,
                 printRequest
                 )
import qualified Data.ByteString.Lazy as LBS  -- pour prettyBody
import           Network.SOAP.Transport.HTTP.TLS ( makeSettings )
import           Data.X509.Validation ( validateDefault )
import           System.X509 ( getSystemCertificateStore )
import           Network.HTTP.Client ( applyBasicAuth )

import           Text.XML.Writer ( XML, node )
import qualified Text.XML as X
import           Data.ByteString ( ByteString )
import           Text.XML.HaXml
                    ( Element,
                      Content(CElem),
                      Document(Document),
                      deep,
                      tagWith,
                      xmlParse )
import           Text.XML.HaXml.Posn ( noPos, Posn )
import           Text.XML.HaXml.Schema.PrimitiveTypes ( runParser, XsdString(XsdString) )
import           Text.XML.HaXml.Schema.Schema ( XMLParser )
import qualified Text.XML.HaXml.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP
import           Data.Yaml  (ToJSON, decodeFileEither)
import           Data.Aeson (FromJSON(..), withObject, (.:))
import           GHC.Generics ( Generic )
import           System.Posix.User
                    ( homeDirectory,
                      getEffectiveUserName,
                      getUserEntryForName )

import           Conso.Fr.Elec.Sge.EnedisDictionnaireResultat
                    ( ResultatType(ResultatType),
                      ResultatLibelleType(ResultatLibelleType),
                      ResultatTypeAttributes(ResultatTypeAttributes,
                                             resultatTypeAttributes_code),
                      ResultatCodeType(ResultatCodeType),
                      elementResultat )


-- | Environnement SGE complet lu depuis @~\/.conso\/conso-env.yaml@.
-- Contient les paramètres de connexion production, homologation et les données de test.
data SgeEnv =
    SgeEnv { production  :: Sge  -- ^ Connexion vers le serveur de production SGE
           , homologation :: Sge  -- ^ Connexion vers le serveur d'homologation SGE
           , test         :: Test -- ^ Données de test (PRM et informations client)
    } deriving (Show,Generic)

-- | Paramètres de connexion à un environnement SGE (production ou homologation).
data Sge =
    Sge { userB2b     :: Text -- ^ Identifiant utilisateur B2B (adresse email)
        , password    :: Text -- ^ Mot de passe B2B
        , contractId  :: Text -- ^ Identifiant de contrat SGE
        , key         :: Text -- ^ Chemin relatif (depuis @~\/.conso\/@) vers la clé privée TLS
        , cert        :: Text -- ^ Chemin relatif (depuis @~\/.conso\/@) vers le certificat client TLS
        , url         :: Text -- ^ URL de base du portail SGE (ex. @https:\/\/sge-portail.enedis.fr@)
    } deriving (Show,Generic)

-- | Données de test PRM et informations client pour le serveur d'homologation.
data Test =
    Test { pointId                            :: Text -- ^ Identifiant PRM de test (14 chiffres)
         , nomClientFinalOuDenominationSociale :: Text -- ^ Nom ou dénomination sociale du client de test
         , numeroEtNomVoie                     :: Text -- ^ Adresse postale (numéro et nom de voie)
         , codePostal                          :: Text -- ^ Code postal
         , codeInseeCommune                    :: Text -- ^ Code INSEE de la commune
    } deriving (Show,Generic)

instance FromJSON SgeEnv
instance ToJSON SgeEnv

instance FromJSON Sge
instance ToJSON Sge

instance FromJSON Test
instance ToJSON Test


-- | Configuration de la requête SOAP associée à un type de webservice.
data ConfigRequest a = ConfigRequest{
          urlSge               :: String         -- ^ Chemin relatif de l'URL du webservice (ex. @\/ConsultationMesures\/v1.1@)
        , soapAction           :: String         -- ^ Valeur de l'en-tête HTTP SOAPAction
        , elementToXMLRequest  :: a -> [Content ()] -- ^ Sérialiseur XML de la requête
}

-- | Configuration du parseur de réponse SOAP associée à un type de webservice.
data ConfigResponse b = ConfigResponse{
          xmlTag          :: String       -- ^ Tag XML racine de la réponse à extraire
        , elementResponse :: XMLParser b  -- ^ Parseur HaXml pour désérialiser la réponse
}

-- | Relie un type de requête à sa 'ConfigRequest' (URL, SOAPAction, sérialiseur XML).
class RequestType a where
    configReq :: ConfigRequest a

-- | Relie un type de réponse à sa 'ConfigResponse' (tag XML, parseur HaXml).
class ResponseType b where
    configResp :: ConfigResponse b


-- | wsRequest permet de réaliser des requêtes vers tous les webservices sur le serveur de production 
--   de SGE. Il est nécessaire de fournir un paramètre de type RequestType dépendant du webservice. 
wsRequest :: (RequestType a, Show a, ResponseType b, Show b) 
          => a  -- ^  request : doit contenir tous les éléments de la requête dépendant du type RequestType
          -> IO ( Either (String, String) b )   -- ^  La réponse ou le code et l'intitulé de l'erreur
wsRequest = sgeRequest True

-- | xmlRequest permet de réaliser des requêtes vers l'ensemble des webservices sur le serveur de 
--   production de SGE et d'obtenir la réponse XML renvoyée par SGE.
xmlRequest :: (RequestType a, Show a) 
           => a -- ^  request : doit contenir tous les éléments de la requête dépendant du type RequestType
           -> IO String -- ^ Renvoit la réponse en XML
xmlRequest = sgeXmlRequest True

-- | Comme 'wsRequest' mais sur le serveur d'homologation.
wsRequestTest :: (RequestType a, Show a, ResponseType b, Show b) => a -> IO ( Either (String, String) b )
wsRequestTest = sgeRequest False

-- | Comme 'xmlRequest' mais sur le serveur d'homologation.
xmlRequestTest :: (RequestType a, Show a) => a -> IO String
xmlRequestTest = sgeXmlRequest False


-- | Lit l'environnement SGE complet depuis @~\/.conso\/conso-env.yaml@.
getEnv :: IO SgeEnv
getEnv = readEnv

-- | Lit les paramètres de connexion pour l'environnement souhaité.
getEnvSge :: Bool  -- ^ @True@ pour la production, @False@ pour l'homologation
          -> IO Sge
getEnvSge prod = do
            env <- readEnv
            if prod then
                return $ production env
            else
                return $ homologation env


myHomeDirectory :: IO String
myHomeDirectory = do
    name <- getEffectiveUserName
    entry <- getUserEntryForName name
    return $ homeDirectory entry

-- | Wrapper interne : lit @sge.enedis@ depuis le fichier YAML fusionné.
newtype ConsoEnvFile = ConsoEnvFile { getSgeEnv :: SgeEnv }

instance FromJSON ConsoEnvFile where
    parseJSON = withObject "top" $ \topObj -> do
        sgeVal <- topObj .: "sge"
        withObject "sge" (\sgeObj -> ConsoEnvFile <$> sgeObj .: "enedis") sgeVal

readEnv :: IO SgeEnv
readEnv = do
    myHD <- myHomeDirectory
    wrapper <- either (error . show) id <$>
        decodeFileEither ( myHD <> "/.conso/conso-env.yaml")
    return (getSgeEnv wrapper)


getLoginContrat :: Bool -> IO (String, String)
getLoginContrat prod = do
    envSge <- getEnvSge prod
    return (T.unpack $ userB2b envSge, T.unpack $ contractId envSge)


sgeRequest :: (RequestType a, Show a, ResponseType b, Show b) => Bool -> a -> IO ( Either (String, String) b )
sgeRequest prod req = do
    let cresp = createConfigResp
    sRequest <- sgeXmlRequest prod req
    xml2hsType (xmlTag cresp) (elementResponse cresp) sRequest


createConfigReq :: (RequestType a) => ConfigRequest a
createConfigReq = configReq

createConfigResp :: (ResponseType b) => ConfigResponse b
createConfigResp = configResp


sgeXmlRequest :: (RequestType a, Show a) => Bool -> a -> IO String
sgeXmlRequest prod req = do
    envSge <- getEnvSge prod
    let creq = createConfigReq
    let xml = PP.render . P.content . head . elementToXMLRequest creq $ req
    let (X.Document _ u _) = X.parseText_ X.def $ L.pack xml
    let xmlConduit =  node . X.NodeElement $ u
    soapRequest envSge (urlSge creq) (soapAction creq) xmlConduit


-- | Like 'tag' but matches by local name, ignoring any namespace prefix.
--   e.g. tagLocal "foo" matches <ns4:foo>, <foo>, <tns:foo>, etc.
tagLocal :: String -> Content i -> [Content i]
tagLocal n = tagWith (\pn -> localPart pn == n)
  where
    localPart s = case dropWhile (/= ':') s of
                    ':':l -> l
                    _     -> s


-- | Met en forme (indente) une réponse XML brute.
prettyXml :: String -> String
prettyXml = PP.render . P.document . xmlParse "(response)"

-- | Comme 'printBody' mais indente le XML avant affichage.
prettyBody :: LBS.ByteString -> IO LBS.ByteString
prettyBody bs = do
    putStrLn "Response :"
    putStrLn $ prettyXml $ L.unpack $ LE.decodeUtf8 bs
    return bs


getHaskellType :: (ResponseType a) => String -> XMLParser a -> Element Posn -> a
getHaskellType myXmlTag myElementResponse root = plans
        where
            cdtcresp = deep (tagLocal myXmlTag) $ CElem root noPos
            toto = runParser myElementResponse cdtcresp
            plans = case fst toto of
                        Right p  -> p
                        Left err -> error $ "getHaskellType: parsing failed for tag '" ++ myXmlTag ++ "': " ++ err


soapRequest :: Sge -> String -> String -> XML -> IO String
soapRequest envSge myUrlSge mySoapAction body = do
    myHD <- myHomeDirectory
    let myHDT = T.pack $ myHD <> "/.conso/"
    let certPath = T.unpack $ T.append myHDT (cert envSge) :: FilePath
    let keyPath = T.unpack $ T.append myHDT (key envSge) :: FilePath
    let fullUrlSge = T.unpack (url envSge) ++ myUrlSge

    systemStore <- getSystemCertificateStore
    settings <- makeSettings (Just certPath) (Just keyPath) (\_ -> validateDefault systemStore)

    let loginUtilisateurBS =  T.encodeUtf8 $ userB2b envSge
    let passwordUtilisateurBS = T.encodeUtf8 $ password envSge

    transport <- initTransportWithM
        settings
        fullUrlSge
        ( withBasicAuth loginUtilisateurBS passwordUtilisateurBS >=> printRequest  ) -- pure or printRequest
        prettyBody -- pure or printBody or prettyBody

    xml <- invokeWS transport mySoapAction () body (RawParser id)
    return $ L.unpack (LE.decodeUtf8 xml)
    where
        withBasicAuth :: ByteString -> ByteString -> RequestProc
        withBasicAuth username passw req = pure (applyBasicAuth username passw req)


xml2hsType :: (ResponseType a) => String -> XMLParser a -> String -> IO (Either (String, String) a)
xml2hsType myXmlTag myElementResponse xml = do
    return $ case checkXMLerror xml of
        (Right root ) -> Right $ getHaskellType myXmlTag myElementResponse root
        (Left (c, l) ) -> Left (c, l)


checkXMLerror :: String -> Either (String, String) (Element Posn)
checkXMLerror xmlResp = do
    let (Document _ _ root _) = xmlParse "(No Document)" xmlResp
    let faultXml    = deep (tagLocal "faultstring") $ CElem root noPos
    let resultatXml = deep (tagLocal "resultat")    $ CElem root noPos
    let resultat    = runParser elementResultat resultatXml
    case faultXml of
        (c:_) -> Left ("SOAP_FAULT", PP.render (P.content c))
        []    -> case resultat of
            (Right ( ResultatType
                      ( ResultatLibelleType ( XsdString _ ) )
                      ( ResultatTypeAttributes{ 
                            resultatTypeAttributes_code = ( ResultatCodeType ( XsdString "SGT200" ) ) 
                        } 
                      )
                  ), _)
                            -> Right root
            (Right ( ResultatType
                      ( ResultatLibelleType ( XsdString l ) )
                      ( ResultatTypeAttributes{ 
                            resultatTypeAttributes_code = ( ResultatCodeType ( XsdString a ) ) 
                        } 
                      )
                  ), _)
                            -> Left (a, l)
            _               -> Right root


