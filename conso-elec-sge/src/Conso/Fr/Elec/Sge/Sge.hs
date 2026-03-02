{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses, AllowAmbiguousTypes #-}

module Conso.Fr.Elec.Sge.Sge  where

import           Control.Monad ( (>=>) )
import qualified Data.Text as T
import           Data.Text.Encoding as T ( encodeUtf8 )
import           Data.Text ( Text )
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE


import           Network.SOAP ( invokeWS, ResponseParser(RawParser) )
import           Network.SOAP.Transport.HTTP ( initTransportWithM, RequestProc,
                 --printRequest, printBody 
                 )
import           Network.SOAP.Transport.HTTP.TLS ( makeSettings )
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
import           Data.Yaml (FromJSON, ToJSON, decodeFileEither)
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


data SgeEnv =
    SgeEnv { production :: Sge
        , homologation :: Sge
        , test :: Test
    } deriving (Show,Generic)

data Sge =
    Sge { userB2b :: Text
        , password :: Text
        , contractId :: Text
        , key :: Text
        , cert :: Text
        , url :: Text
    } deriving (Show,Generic)

data Test =
    Test { pointId :: Text
        , nomClientFinalOuDenominationSociale :: Text
        , numeroEtNomVoie :: Text
        , codePostal :: Text
        , codeInseeCommune :: Text
    } deriving (Show,Generic)

instance FromJSON SgeEnv
instance ToJSON SgeEnv

instance FromJSON Sge
instance ToJSON Sge

instance FromJSON Test
instance ToJSON Test


data ConfigRequest a = ConfigRequest{
          urlSge :: String
        , soapAction :: String
        , elementToXMLRequest :: a -> [Content ()]
}

data ConfigResponse b = ConfigResponse{
          xmlTag :: String
        , elementResponse :: XMLParser b
}

class RequestType a where
    configReq :: ConfigRequest a

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

-- | wsRequestTest est utilisé pour faire des tests sur le serveur d'homologation
wsRequestTest :: (RequestType a, Show a, ResponseType b, Show b) => a -> IO ( Either (String, String) b )
wsRequestTest = sgeRequest False

-- | xmlRequestTest est utilisé pour faire des tests sur le serveur d'homologation
xmlRequestTest :: (RequestType a, Show a) => a -> IO String
xmlRequestTest = sgeXmlRequest False


getEnv :: IO SgeEnv
getEnv = readEnv

getEnvSge :: Bool -> IO Sge
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

readEnv :: IO SgeEnv
readEnv = do
    myHD <- myHomeDirectory
    either (error . show) id <$>
        decodeFileEither ( myHD <> "/.conso/conso-elec-sge-env.yaml")


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

    --settings <- makeSettings (Just "production.crt") (Just "production.key") validateDefault
    settings <- makeSettings (Just certPath) (Just keyPath) (\_ _ _ _ -> return [])

    let loginUtilisateurBS =  T.encodeUtf8 $ userB2b envSge
    let passwordUtilisateurBS = T.encodeUtf8 $ password envSge

    transport <- initTransportWithM
        settings
        fullUrlSge
        ( withBasicAuth loginUtilisateurBS passwordUtilisateurBS >=> pure  ) -- or printRequest
        pure -- or printBody

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
checkXMLerror xmlResp =  do
    let (Document _ _ root _) = xmlParse "(No Document)" xmlResp
    let resultatXml = deep (tagLocal "resultat") $ CElem root noPos
    let resultat = runParser elementResultat resultatXml

    case resultat of
        (Right ( ResultatType
                  ( ResultatLibelleType ( XsdString _ ) )
                  ( ResultatTypeAttributes{ resultatTypeAttributes_code = ( ResultatCodeType ( XsdString "SGT200" ) ) } )
              ), _)
                        -> Right root


        (Right ( ResultatType
                  ( ResultatLibelleType ( XsdString l ) )
                  ( ResultatTypeAttributes{ resultatTypeAttributes_code = ( ResultatCodeType ( XsdString a ) ) } )
              ), _)
                        -> Left (a, l)
        _               -> Right root


