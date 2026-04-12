{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.Adict.Preuves
Description : Webservice GRDF ADICT — POST \/droit_acces\/{id_droit_acces}\/preuves

Permet de soumettre une preuve d'accord client pour un droit d'accès en
attente de justificatif.  Le fichier (PDF ou image JPEG\/PNG) est envoyé
en @multipart\/form-data@ vers l'API GRDF ADICT.

Limite : 4 Mo maximum par fichier.

Usage :

> session <- initSession False
> rep     <- soumettrePrevue session "3044b042-2f6a-4172-9a75-b7e1bbbb0cfd" "/tmp/accord_client.pdf"
> case rep of
>     Left  err -> print err
>     Right ()  -> putStrLn "Preuve soumise avec succès"
-}
module Conso.Fr.Gaz.Adict.Preuves
  ( soumettrePrevue
  , soumettrePrevueSandbox
  ) where

import           Control.Exception              ( try, SomeException )
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Network.HTTP.Client
import           Network.HTTP.Types.Status      ( statusCode )
import           System.FilePath                ( takeFileName )

import           Conso.Fr.Gaz.Adict.Adict


-- | Soumet une preuve d'accord client pour un droit d'accès
--   (@POST \/droit_acces\/{id_droit_acces}\/preuves@).
--
-- Envoie le fichier désigné par 'FilePath' en @multipart\/form-data@.
-- Retourne @Right ()@ en cas de succès (HTTP 200\/201\/204) ou une
-- 'AdictError' sinon.
soumettrePrevue
    :: AdictSession
    -> Text      -- ^ UUID du droit d'accès
    -> FilePath  -- ^ Chemin vers le fichier de preuve (PDF\/image, max 4 Mo)
    -> IO (Either AdictError ())
soumettrePrevue session idDroitAcces filePath = do
    tokResult <- getBearerToken session
    case tokResult of
        Left e -> return $ Left e
        Right tok -> do
            fileBytes <- BS.readFile filePath
            let boundary    = "ADict-Proof-Boundary-01234567"
            let body        = buildMultipartBody boundary (takeFileName filePath) fileBytes
            let contentType = "multipart/form-data; boundary=" <> boundary
            let url         = buildUrl (sessionConfig session)
                                ("/droit_acces/" <> T.unpack idDroitAcces <> "/preuves")
            initReq <- parseRequest url
            let req = initReq
                    { method         = "PUT"
                    , requestBody    = RequestBodyLBS body
                    , requestHeaders =
                        [ ("Authorization", "Bearer " <> T.encodeUtf8 tok)
                        , ("Content-Type",  contentType)
                        ]
                    }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st = statusCode (responseStatus resp)
                    in if st `elem` [200, 201, 204]
                       then return $ Right ()
                       else return $ Left
                                (HttpError st
                                    (T.decodeUtf8 (LBS.toStrict (responseBody resp))))


-- | Comme 'soumettrePrevue' avec une session bac à sable auto-initialisée.
soumettrePrevueSandbox :: Text -> FilePath -> IO (Either AdictError ())
soumettrePrevueSandbox idDroitAcces filePath = do
    session <- initSession False False False
    soumettrePrevue session idDroitAcces filePath


-- ---------------------------------------------------------------------------
-- Utilitaire interne

-- | Construit un corps @multipart\/form-data@ minimal avec un seul champ
--   @preuve@ contenant le fichier.
buildMultipartBody :: BS.ByteString -> FilePath -> BS.ByteString -> LBS.ByteString
buildMultipartBody boundary fileName fileBytes = LBS.fromStrict $
       "--" <> boundary <> "\r\n"
    <> "Content-Disposition: form-data; name=\"preuve\"; filename=\""
    <> BSC.pack fileName <> "\"\r\n"
    <> "Content-Type: application/pdf\r\n"
    <> "\r\n"
    <> fileBytes
    <> "\r\n"
    <> "--" <> boundary <> "--\r\n"
