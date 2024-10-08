{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Conso.Fr.Elec.Sge.Sge  where

import           Control.Monad ( (>=>) )
import qualified Data.Text as T
import           Data.Text.Encoding as T ( encodeUtf8 )
import           Data.Text ( Text )
import qualified Data.Text.Lazy as L
import           Text.Pretty.Simple (pPrint)


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
                      tag,
                      xmlParse )
import           Text.XML.HaXml.Posn ( noPos, Posn )
import           Text.XML.HaXml.Schema.PrimitiveTypes ( runParser, XsdString(XsdString) )
import           Text.XML.HaXml.Schema.Schema ( XMLParser )
import qualified Text.XML.HaXml.Pretty as P
import qualified Text.PrettyPrint.HughesPJ as PP
import           Data.ByteString.Lazy.Char8 ( unpack )
import           Data.Yaml (FromJSON, ToJSON, decodeFileEither)
import           GHC.Generics ( Generic )
import           System.Posix.User
                    ( UserEntry(homeDirectory),
                      getEffectiveUserName,
                      getUserEntryForName )

import           Conso.Fr.Elec.Sge.EnedisDictionnaireResultat
                    ( ResultatType(ResultatType),
                      ResultatLibelleType(ResultatLibelleType),
                      ResultatTypeAttributes(ResultatTypeAttributes,
                                             resultatTypeAttributes_code),
                      ResultatCodeType(ResultatCodeType),
                      elementResultat )


data Env =
    Env { production :: Sge
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

instance FromJSON Env
instance ToJSON Env

instance FromJSON Sge
instance ToJSON Sge

instance FromJSON Test
instance ToJSON Test

data ConfigWS a b = ConfigWS{
          urlSge :: String
        , soapAction :: String
        , elementToXMLRequest :: a -> [Content ()]
        , xmlTag :: String
        , elementResponse :: XMLParser b
}

class RequestType a where
class ResponseType a


getEnv :: IO Env
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

readEnv :: IO Env
readEnv = do
    myHD <- myHomeDirectory
    either (error . show) id <$>
        decodeFileEither ( myHD <> "/.conso/conso-elec-sge-env.yaml")


getLoginContrat :: Bool -> IO (String, String)
getLoginContrat prod = do
    envSge <- getEnvSge prod
    return (T.unpack $ userB2b envSge, T.unpack $ contractId envSge)


sgeRequest :: (RequestType a, Show a, ResponseType b, Show b) => Bool -> a -> ConfigWS a b -> IO ( Either (String, String) b )
sgeRequest prod req config = do
    envSge <- getEnvSge prod
    let xml = PP.render . P.content . head . elementToXMLRequest config $ req
    let (X.Document _ u _) = X.parseText_ X.def $ L.pack xml
    let xmlConduit =  node . X.NodeElement $ u
    --print $ urlSge config
    --print $ soapAction config
    --pPrint req
    --pPrint xmlConduit
    sRequest <- soapRequest envSge (urlSge config) (soapAction config) xmlConduit
    --putStrLn sRequest
    --hsType <- xml2hsType (xmlTag config) (elementResponse config) sRequest
    xml2hsType (xmlTag config) (elementResponse config) sRequest
    --case hsType of
    --    Right resp -> pPrint resp
    --    Left (c, l) -> do
    --        putStr "Erreur "
    --        putStrLn c
    --        putStrLn l


getHaskellType :: (ResponseType a) => String -> XMLParser a -> Element Posn -> a
getHaskellType myXmlTag myElementResponse root = plans
        where
            cdtcresp = deep (tag myXmlTag) $ CElem root noPos
            toto = runParser myElementResponse cdtcresp
            (Right plans) = fst toto


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
    return $ unpack xml
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
    let resultatXml = deep (tag "resultat") $ CElem root noPos
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


