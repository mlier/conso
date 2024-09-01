{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Sge ( consultationDonneesTechniquesContractuellesRequest ) where

import           Control.Monad ( (>=>) )
import qualified Data.Text as T
import           Data.Text.Encoding as T ( encodeUtf8 )
import           Data.Text ( Text ) 
import           Text.Pretty.Simple (pPrint)


import           Network.SOAP ( invokeWS, ResponseParser(RawParser) )
import           Network.SOAP.Transport.HTTP ( initTransportWithM, RequestProc )
import           Network.SOAP.Transport.HTTP.TLS ( makeSettings )
import           Network.HTTP.Client ( applyBasicAuth )

import           Text.XML.Writer ( elementA, element, XML )
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
import           Data.ByteString.Lazy.Char8 ( unpack ) 
import           Data.Yaml (FromJSON, ToJSON, decodeFileEither)
import           GHC.Generics ( Generic )
import           System.Posix.User
                    ( UserEntry(homeDirectory),
                      getEffectiveUserName,
                      getUserEntryForName ) 

import           ConsulterDonneesTechniquesContractuellesV10
                    ( ConsulterDonneesTechniquesContractuellesResponseType,
                      elementConsulterDonneesTechniquesContractuellesResponse )
import           EnedisDictionnaireResultat
                    ( ResultatType(ResultatType),
                      ResultatLibelleType(ResultatLibelleType),
                      ResultatTypeAttributes(ResultatTypeAttributes,
                                             resultatTypeAttributes_code),
                      ResultatCodeType(ResultatCodeType),
                      elementResultat )


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
withBasicAuth username passw request = pure (applyBasicAuth username passw request)

--getSge :: IO ()
--getSge = do
--    env <- readEnv
--    let sgeEnv = sge env
--    --print env
--    --print sgeEnv
--
--    myHD <- myHomeDirectory
--    let myHDT = T.pack $ myHD <> "/.conso/"
--    let certPath = T.unpack $ T.append myHDT (cert sgeEnv) :: FilePath
--    let keyPath = T.unpack $ T.append myHDT (key sgeEnv) :: FilePath
--
--    --settings <- makeSettings (Just "production-coach-energy.crt") (Just "production-coach-energy.key") validateDefault
--    settings <- makeSettings (Just certPath) (Just keyPath) (\_ _ _ _ -> return [])
--    let pointId = "21429667044956000" :: Text
--    let loginUtilisateur = userB2b sgeEnv
--    let passwordUtilisateur = password sgeEnv
--    let autorisationClient = True
--
--    let loginUtilisateurBS =  T.encodeUtf8 loginUtilisateur
--    let passwordUtilisateurBS = T.encodeUtf8 passwordUtilisateur
--
--    let urlSge = "https://sge-b2b.enedis.fr/ConsultationDonneesTechniquesContractuelles/v1.0"
--    let soapAction =  urlSge
--    let body = elementA "tns:consulterDonneesTechniquesContractuelles"
--                        [("xmlns:tns", "http://www.enedis.fr/sge/b2b/services/consulterdonneestechniquescontractuelles/v1.0")]
--                        $ do
--                                Text.XML.Writer.element "pointId" pointId
--                                Text.XML.Writer.element "loginUtilisateur" loginUtilisateur
--                                Text.XML.Writer.element "autorisationClient" autorisationClient
--    putStrLn urlSge
--
--    transport <- initTransportWithM
--        settings
--        urlSge
--        ( withBasicAuth loginUtilisateurBS passwordUtilisateurBS >=> pure  ) -- or printRequest
--        pure -- or printBody
--
--    response <- invokeWS transport soapAction () body (RawParser id)
--    --print response
--
--    let responseStr = unpack response
--    cdtcr responseStr
--    --where
--    --    parser cur = cur $// laxElement "consulterDonneesTechniquesContractuellesResponse"
--
--
--titi root = adresseInstallationType_escalierEtEtageEtAppartement
--                                    $ pointDonneesGeneralesType_adresseInstallation
--                                    $ pointType_donneesGenerales
--                                    $ consulterDonneesTechniquesContractuellesResponseType_point plans
--    where
--      cdtcresp = deep (tag "ns7:consulterDonneesTechniquesContractuellesResponse") $ CElem root noPos
--      toto = runParser elementConsulterDonneesTechniquesContractuellesResponse [head cdtcresp]
--      (Right plans) = fst toto



--cdtcr :: String -> IO ()
--cdtcr s = do
--    let (Document _ _ root _) = xmlParse "(No Document)" s
--    let resultatXml = deep (tag "resultat") $ CElem root noPos
--    --print resultatXml
--    let resultat = runParser elementResultat resultatXml
--    --print resultat
--    --let cdtcresp = deep (tag "ns7:consulterDonneesTechniquesContractuellesResponse") $ CElem root noPos
--    --print root
--    --let toto = runParser elementConsulterDonneesTechniquesContractuellesResponse [head cdtcresp]
--    --print toto
--    --let (Right plans) = fst toto
--    --print plans
--    print $ case resultat of
--      (Right ( ResultatType 
--                  ( ResultatLibelleType ( XsdString l ) ) 
--                  ( ResultatTypeAttributes{ resultatTypeAttributes_code = ( ResultatCodeType ( XsdString "SGT200" ) ) } )
--              ), _) 
--                        -> Left root
--                        
--                          
--      (Right ( ResultatType 
--                  ( ResultatLibelleType ( XsdString l ) ) 
--                  ( ResultatTypeAttributes{ resultatTypeAttributes_code = ( ResultatCodeType ( XsdString a ) ) } )
--              ), _)
--                        -> Right (a, l)
--      _                 -> Right ("BAD", "Erreur interne")
--
--    --let (Right r) = fst resultat
--
-----------------------------------------------------------



consultationDonneesTechniquesContractuellesRequest :: Text -> Bool -> IO ()
consultationDonneesTechniquesContractuellesRequest pointId autorisationClient = do 
    env <- readEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv

    let body = elementA "tns:consulterDonneesTechniquesContractuelles"
                        [("xmlns:tns", "http://www.enedis.fr/sge/b2b/services/consulterdonneestechniquescontractuelles/v1.0")]
                        $ do
                                Text.XML.Writer.element "pointId" pointId
                                Text.XML.Writer.element "loginUtilisateur" loginUtilisateur
                                Text.XML.Writer.element "autorisationClient" autorisationClient
    let urlSge = "https://sge-b2b.enedis.fr/ConsultationDonneesTechniquesContractuelles/v1.0"
    let soapAction =  urlSge
    sRequest <- soapRequest sgeEnv urlSge soapAction body
    case sRequest of 
        Left resp -> pPrint resp
        Right (c, l) -> do 
            putStr "Erreur "
            putStrLn c
            putStrLn l


soapRequest :: Sge -> String -> String -> XML -> IO (Either ConsulterDonneesTechniquesContractuellesV10.ConsulterDonneesTechniquesContractuellesResponseType (String, String) )
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

    response <- invokeWS transport soapAction () body (RawParser id)
    return $ case checkXMLerror $ unpack response of
        (Left root ) -> Left $ getHaskellType "ns7:consulterDonneesTechniquesContractuellesResponse" 
                                               ConsulterDonneesTechniquesContractuellesV10.elementConsulterDonneesTechniquesContractuellesResponse root
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


getHaskellType :: String -> XMLParser ConsulterDonneesTechniquesContractuellesV10.ConsulterDonneesTechniquesContractuellesResponseType ->Element Posn -> ConsulterDonneesTechniquesContractuellesV10.ConsulterDonneesTechniquesContractuellesResponseType
getHaskellType xmlTag elementResponse root = plans
    where
      cdtcresp = deep (tag xmlTag) $ CElem root noPos
      toto = runParser elementResponse [head cdtcresp]
      (Right plans) = fst toto