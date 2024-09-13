{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Sge  where

import           Control.Monad ( (>=>) )
import qualified Data.Text as T
import           Data.Text.Encoding as T ( encodeUtf8 )
import           Data.Text ( Text )
import qualified Data.Text.Lazy as L
import           Text.Pretty.Simple (pPrint)


import           Network.SOAP ( invokeWS, ResponseParser(RawParser) )
import           Network.SOAP.Transport.HTTP ( initTransportWithM, RequestProc, printRequest, printBody )
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
import           Text.XML.HaXml.Schema.PrimitiveTypes ( runParser, XsdString(XsdString), Boolean )
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

import           EnedisDictionnaireResultat
                    ( ResultatType(ResultatType),
                      ResultatLibelleType(ResultatLibelleType),
                      ResultatTypeAttributes(ResultatTypeAttributes,
                                             resultatTypeAttributes_code),
                      ResultatCodeType(ResultatCodeType),
                      elementResultat )


getEnv :: IO Env
getEnv = readEnv


newtype Env =
    Env { sge :: Sge
    } deriving (Show,Generic)

data Sge =
    Sge { userB2b :: Text
        , password :: Text
        , contractId :: Text
        , key :: Text
        , cert :: Text
        , url :: Text
    } deriving (Show,Generic)

instance FromJSON Env
instance ToJSON Env

instance FromJSON Sge
instance ToJSON Sge

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

withBasicAuth :: ByteString -> ByteString -> RequestProc
withBasicAuth username passw req = pure (applyBasicAuth username passw req)


--consulterMesuresDetailleesV3Request :: Text -> IO()
--consulterMesuresDetailleesV3Request pointId = do

    --hsType <- xml2hsType "ns4:rechercherServicesSouscritsMesuresResponse" elementRechercherServicesSouscritsMesuresResponse sRequest
    

sgeRequest :: (RequestType a, Show a, ResponseType b, Show b) => a -> XMLParser b -> IO ()
sgeRequest req elementResponse= do 
    env <- readEnv
    let sgeEnv = sge env
    let (urlSge, soapAction, elementToXMLRequest, xmlTag) = config req
    let t = PP.render . P.content . head . elementToXMLRequest $ req
    let (X.Document _ u _) = X.parseText_ X.def $ L.pack t
    let v =  node . X.NodeElement $ u
    print urlSge
    print soapAction
    pPrint req
    pPrint v
    sRequest <- soapRequest sgeEnv urlSge soapAction v
    putStrLn sRequest
    hsType <- xml2hsType xmlTag elementResponse sRequest
    case hsType of
        Left resp -> pPrint resp
        Right (c, l) -> do
            putStr "Erreur "
            putStrLn c
            putStrLn l

class RequestType a where
   config :: a -> ( String, String, a -> [Content ()], String )

class ResponseType a


getHaskellType :: (ResponseType a) => String -> XMLParser a -> Element Posn -> a
getHaskellType xmlTag elementResponse root = plans
        where
            cdtcresp = deep (tag xmlTag) $ CElem root noPos
            toto = runParser elementResponse [head cdtcresp]
            (Right plans) = fst toto


soapRequest :: Sge -> String -> String -> XML -> IO String
soapRequest sgeEnv urlSge soapAction body = do
    myHD <- myHomeDirectory
    let myHDT = T.pack $ myHD <> "/.conso/"
    let certPath = T.unpack $ T.append myHDT (cert sgeEnv) :: FilePath
    let keyPath = T.unpack $ T.append myHDT (key sgeEnv) :: FilePath

    --settings <- makeSettings (Just "production-coach-energy.crt") (Just "production-coach-energy.key") validateDefault
    settings <- makeSettings (Just certPath) (Just keyPath) (\_ _ _ _ -> return [])

    let loginUtilisateurBS =  T.encodeUtf8 $ userB2b sgeEnv
    let passwordUtilisateurBS = T.encodeUtf8 $ password sgeEnv

    transport <- initTransportWithM
        settings
        urlSge
        ( withBasicAuth loginUtilisateurBS passwordUtilisateurBS >=> pure  ) -- or printRequest
        pure -- or printBody

    xml <- invokeWS transport soapAction () body (RawParser id)
    return $ unpack xml


xml2hsType :: (ResponseType a) => String -> XMLParser a -> String -> IO (Either a (String, String) )
xml2hsType xmlTag elementResponse xml = do
    return $ case checkXMLerror xml of
        (Left root ) -> Left $ getHaskellType xmlTag elementResponse root
        (Right (c, l) ) -> Right (c, l)


checkXMLerror :: String -> Either (Element Posn) (String, String)
checkXMLerror xmlResp =  do
    let (Document _ _ root _) = xmlParse "(No Document)" xmlResp
    let resultatXml = deep (tag "resultat") $ CElem root noPos
    let resultat = runParser elementResultat resultatXml

    case resultat of
        (Right ( ResultatType
                  ( ResultatLibelleType ( XsdString l ) )
                  ( ResultatTypeAttributes{ resultatTypeAttributes_code = ( ResultatCodeType ( XsdString "SGT200" ) ) } )
              ), _)
                        -> Left root


        (Right ( ResultatType
                  ( ResultatLibelleType ( XsdString l ) )
                  ( ResultatTypeAttributes{ resultatTypeAttributes_code = ( ResultatCodeType ( XsdString a ) ) } )
              ), _)
                        -> Right (a, l)
        _               -> Right ("BAD", "Erreur interne")


