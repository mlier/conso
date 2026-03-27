{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-|
Module      : Conso.Fr.Gaz.Adict.Adict
Description : Infrastructure OAuth2 et HTTP commune à tous les services ADICT GRDF B2B

Fournit :

  * Les types de configuration 'AdictEnv', 'Adict' et 'TestData' lus depuis
    @~\/.conso\/conso-env.yaml@ (section @gaz.adict@)
  * La gestion du token OAuth2 (Client Credentials flow) via 'hoauth2',
    avec mise en cache automatique dans un 'IORef'
  * La structure 'AdictSession' qui encapsule la configuration, le token et
    le 'Manager' HTTP partagé
  * Les fonctions bas niveau 'adictGet', 'adictGetNDJSON', 'adictPut',
    'adictPost' et 'adictPatch' avec Bearer token automatique
  * 'runAdictClient' pour exécuter des actions 'Servant.Client.ClientM'
    dans le contexte d'une session ADICT

Le transport utilise HTTPS (TLS) via 'newTlsManager'.
-}
module Conso.Fr.Gaz.Adict.Adict
  ( -- * Types de configuration
    AdictEnv(..)
  , Adict(..)
  , TestData(..)
    -- * Session
  , AdictSession(..)
  , initSession
  , initSessionWith
    -- * Erreurs
  , AdictError(..)
    -- * Lecture de la configuration
  , getEnv
  , getEnvAdict
    -- * Token
  , getBearerToken
    -- * Requêtes HTTP (bas niveau)
  , adictGet
  , adictGetNDJSON
  , adictGetNDJSONStream
  , adictGetRaw
  , adictPut
  , adictPost
  , adictPostNDJSON
  , adictPatch
    -- * Client Servant
  , runAdictClient
    -- * Utilitaires
  , buildUrl
  , parseNDJSON
  , myHomeDirectory
  ) where

import           Control.Exception                              ( try, SomeException )
import           Control.Monad.Trans.Except                    ( runExceptT, ExceptT )
import           Data.Aeson
import qualified Data.ByteString.Lazy                           as LBS
import           Conduit                                        ( yieldMany )
import           Data.Conduit                                   ( ConduitT )
import           Data.IORef
import qualified Data.Map.Strict                                as Map
import qualified Data.Set                                       as Set
import qualified Data.Text                                      as T
import           Data.Text                                      ( Text )
import qualified Data.Text.Encoding                             as T
import qualified Data.Text.Lazy                                 as TL
import           Data.Time
import           Data.Yaml                                      ( decodeFileEither )
import           GHC.Generics                                   ( Generic )
import qualified Data.ByteString.Char8                          as BSC
import qualified Data.ByteString.Lazy.Char8                     as LBSC
import qualified Data.CaseInsensitive                           as CI
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
                    ( newTlsManager, newTlsManagerWith, tlsManagerSettings )
import           Network.HTTP.Types.Status                      ( statusCode )
import           System.IO                                      ( hPutStrLn, stderr )
import           Network.OAuth2.Experiment
                    ( conduitTokenRequest
                    , Idp(..), IdpApplication(..)
                    , ClientId(..), ClientSecret(..), Scope(..)
                    , ClientAuthenticationMethod(..), NoNeedExchangeToken(..) )
import           Network.OAuth2.Experiment.Grants.ClientCredentials
                    ( ClientCredentialsApplication(..) )
import           Network.OAuth2.TokenRequest                    ( TokenResponse(..), TokenResponseError )
import           Network.OAuth2                                 ( AccessToken(..) )
import           Servant.Client
                    ( ClientM, ClientEnv(..), ClientError(..)
                    , mkClientEnv, runClientM, parseBaseUrl )
import qualified Servant.Client.Core                            as SCC
import           System.Posix.User
                    ( homeDirectory
                    , getEffectiveUserName
                    , getUserEntryForName )
import qualified URI.ByteString                                 as UB


-- ---------------------------------------------------------------------------
-- Types de configuration

-- | Environnement ADICT complet lu depuis @~\/.conso\/conso-env.yaml@.
data AdictEnv = AdictEnv
    { sandbox    :: Adict    -- ^ Connexion vers le bac à sable GRDF
    , production :: Adict    -- ^ Connexion vers le serveur de production GRDF
    , testData   :: TestData -- ^ Données de test (PCE de bac à sable)
    } deriving (Show, Generic)

-- | Paramètres de connexion à un environnement ADICT (sandbox ou production).
data Adict = Adict
    { clientId     :: Text -- ^ Identifiant client OAuth2
    , clientSecret :: Text -- ^ Secret client OAuth2
    , tokenUrl     :: Text -- ^ URL du serveur d'autorisation (SSO GRDF)
    , adictUrl     :: Text -- ^ URL de base de l'API ADICT
    } deriving (Show, Generic)

-- | Données de test pour le bac à sable GRDF.
newtype TestData = TestData
    { testPce :: Text -- ^ Identifiant PCE de test (14 chiffres ou GI + 6 chiffres)
    } deriving (Show, Generic)

instance FromJSON AdictEnv
instance ToJSON   AdictEnv
instance FromJSON Adict
instance ToJSON   Adict
instance FromJSON TestData
instance ToJSON   TestData


-- ---------------------------------------------------------------------------
-- Token OAuth2

-- | Token en cache avec sa date d'expiration.
data TokenState = TokenState
    { tokenValue     :: Text
    , tokenExpiresAt :: UTCTime
    }

-- ---------------------------------------------------------------------------
-- Session

-- | Session ADICT encapsulant la configuration, le cache de token et le Manager HTTP.
data AdictSession = AdictSession
    { sessionConfig  :: Adict
    , sessionToken   :: IORef (Maybe TokenState)
    , sessionManager :: Manager
    , sessionDebug   :: Bool   -- ^ @True@ = log des requêtes HTTP sur stderr
    }

-- | Initialise une session ADICT en lisant la configuration depuis
--   @~\/.conso\/conso-env.yaml@.
--   Premier @Bool@ : @True@ = production, @False@ = bac à sable.
--   Second @Bool@  : @True@ = mode debug (log HTTP sur stderr).
initSession :: Bool -> Bool -> IO AdictSession
initSession prod debug = do
    cfg <- getEnvAdict prod
    initSessionWith debug cfg

-- | Initialise une session ADICT avec une configuration explicite.
--   Le @Bool@ active le mode debug (log HTTP sur stderr).
initSessionWith :: Bool -> Adict -> IO AdictSession
initSessionWith debug cfg = do
    tokenRef <- newIORef Nothing
    mgr      <- if debug then newDebugTlsManager else newTlsManager
    return $ AdictSession cfg tokenRef mgr debug

-- | Manager TLS qui écrit chaque requête (méthode, URL, en-têtes, corps)
--   sur stderr avant de l'envoyer.  Utile pour diagnostiquer les erreurs OAuth2.
newDebugTlsManager :: IO Manager
newDebugTlsManager = newTlsManagerWith tlsManagerSettings
    { managerModifyRequest = \req -> do
        let scheme = if secure req then "https" else "http" :: String
        hPutStrLn stderr $ "[DEBUG] " <> BSC.unpack (method req)
                        <> " " <> scheme
                        <> "://" <> BSC.unpack (host req)
                        <> ":" <> show (port req)
                        <> BSC.unpack (path req)
                        <> BSC.unpack (queryString req)
        mapM_ (\(k, v) -> hPutStrLn stderr $
                    "[DEBUG]   " <> BSC.unpack (CI.original k)
                    <> ": " <> BSC.unpack v)
              (requestHeaders req)
        case requestBody req of
            RequestBodyLBS  lbs -> hPutStrLn stderr $ "[DEBUG] body: " <> LBSC.unpack lbs
            RequestBodyBS   bs  -> hPutStrLn stderr $ "[DEBUG] body: " <> BSC.unpack bs
            _                   -> hPutStrLn stderr   "[DEBUG] body: <streaming>"
        return req
    }


-- ---------------------------------------------------------------------------
-- Erreurs

-- | Type d'erreur retourné par toutes les fonctions de requête ADICT.
data AdictError
    = HttpError   Int  Text  -- ^ Erreur HTTP : code de statut + corps de la réponse
    | ParseError       Text  -- ^ Erreur de décodage JSON
    | AuthError        Text  -- ^ Erreur d'authentification OAuth2
    | NetworkError     Text  -- ^ Erreur réseau ou configuration invalide
    deriving (Show)


-- ---------------------------------------------------------------------------
-- Lecture de la configuration

-- | Chemin du répertoire personnel de l'utilisateur courant.
myHomeDirectory :: IO String
myHomeDirectory = do
    name  <- getEffectiveUserName
    entry <- getUserEntryForName name
    return $ homeDirectory entry

-- | Wrapper interne pour décoder la section @gaz.adict@ du fichier YAML.
newtype ConsoEnvFile = ConsoEnvFile { getAdictEnv :: AdictEnv }

instance FromJSON ConsoEnvFile where
    parseJSON = withObject "top" $ \top -> do
        adict <- top .: "adict"
        withObject "adict" (\g -> ConsoEnvFile <$> g .: "grdf") adict

-- | Lit l'environnement ADICT complet depuis @~\/.conso\/conso-env.yaml@.
getEnv :: IO AdictEnv
getEnv = do
    hd      <- myHomeDirectory
    wrapper <- either (error . show) id <$>
                   decodeFileEither (hd <> "/.conso/conso-env.yaml")
    return (getAdictEnv wrapper)

-- | Lit les paramètres de connexion pour l'environnement souhaité.
--   @True@ = production, @False@ = bac à sable.
getEnvAdict :: Bool -> IO Adict
getEnvAdict prod = do
    env <- getEnv
    return $ if prod then production env else sandbox env


-- ---------------------------------------------------------------------------
-- Gestion du token (hoauth2)

-- | Récupère un Bearer token valide, en utilisant le cache ou en en demandant
--   un nouveau si le token est expiré ou absent.
getBearerToken :: AdictSession -> IO (Either AdictError Text)
getBearerToken session = do
    mTok <- readIORef (sessionToken session)
    now  <- getCurrentTime
    case mTok of
        Just t | tokenExpiresAt t > now -> return $ Right (tokenValue t)
        _ -> do
            result <- fetchToken (sessionManager session) (sessionConfig session)
            case result of
                Left  e   -> return $ Left e
                Right tok -> do
                    writeIORef (sessionToken session) (Just tok)
                    return $ Right (tokenValue tok)

-- | Appelle le serveur SSO GRDF via hoauth2 (Client Credentials flow) pour
--   obtenir un nouveau token.  Supporte @ClientSecretPost@ (GRDF) et
--   @ClientSecretBasic@ (SOFIT) selon la valeur de 'tokenUrl'.
fetchToken :: Manager -> Adict -> IO (Either AdictError TokenState)
fetchToken mgr cfg =
    case UB.parseURI UB.strictURIParserOptions (T.encodeUtf8 (tokenUrl cfg)) of
        Left  e   -> return $ Left (AuthError $ "URL SSO invalide : " <> T.pack (show e))
        Right uri -> do
            let idpApp = IdpApplication
                    { idp = Idp
                        { idpUserInfoEndpoint            = uri
                        , idpAuthorizeEndpoint           = uri
                        , idpTokenEndpoint               = uri
                        , idpDeviceAuthorizationEndpoint = Nothing
                        }
                    , application = ClientCredentialsApplication
                        { ccClientId                   = ClientId  (TL.fromStrict (clientId     cfg))
                        , ccClientSecret               = ClientSecret (TL.fromStrict (clientSecret cfg))
                        , ccName                       = TL.fromStrict (clientId cfg)
                        , ccScope                      = Set.fromList [Scope "/adict/bas/v3"]
                        , ccTokenRequestExtraParams    = Map.empty
                        , ccClientAuthenticationMethod = ClientSecretPost
                        }
                    }
            result <- runExceptT
                        (conduitTokenRequest idpApp mgr NoNeedExchangeToken
                            :: ExceptT TokenResponseError IO TokenResponse)
            case result of
                Left  e   -> return $ Left (AuthError (T.pack (show e)))
                Right tok -> do
                    now <- getCurrentTime
                    let expIn     = maybe 3600 id (expiresIn tok)
                    let expiresAt = addUTCTime (fromIntegral (expIn - 100)) now
                    return $ Right TokenState
                        { tokenValue     = atoken (accessToken tok)
                        , tokenExpiresAt = expiresAt
                        }


-- ---------------------------------------------------------------------------
-- Client Servant

-- | Exécute une action 'ClientM' dans le contexte d'une session ADICT.
--   Injecte automatiquement le Bearer token OAuth2 dans chaque requête.
runAdictClient :: AdictSession -> ClientM a -> IO (Either AdictError a)
runAdictClient session action = do
    tokResult <- getBearerToken session
    case tokResult of
        Left e -> return $ Left e
        Right tok -> do
            let bearer = "Bearer " <> T.encodeUtf8 tok
            eBaseUrl <- try (parseBaseUrl (T.unpack (adictUrl (sessionConfig session))))
                            :: IO (Either SomeException SCC.BaseUrl)
            case eBaseUrl of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right burl -> do
                    let env0 = mkClientEnv (sessionManager session) burl
                    let env  = env0 { makeClientRequest = \bu req -> do
                            req' <- makeClientRequest env0 bu req
                            return $ req' { requestHeaders =
                                ("Authorization", bearer) : requestHeaders req' }
                            }
                    result <- runClientM action env
                    return $ either (Left . toAdictError) Right result

-- | Convertit une erreur Servant en 'AdictError'.
toAdictError :: ClientError -> AdictError
toAdictError (FailureResponse _ r) = HttpError
    (statusCode (SCC.responseStatusCode r))
    (decodeBody (SCC.responseBody r))
toAdictError (DecodeFailure msg _) = ParseError msg
toAdictError (ConnectionError e)   = NetworkError (T.pack (show e))
toAdictError other                 = NetworkError (T.pack (show other))


-- ---------------------------------------------------------------------------
-- Construction des URLs

-- | Construit l'URL complète à partir de la configuration et d'un chemin relatif.
buildUrl :: Adict -> String -> String
buildUrl cfg apiPath = T.unpack (adictUrl cfg) <> apiPath


-- ---------------------------------------------------------------------------
-- Requêtes HTTP

-- | GET retournant un objet JSON unique.
adictGet :: FromJSON a => AdictSession -> String -> IO (Either AdictError a)
adictGet session apiPath = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { requestHeaders = [("Authorization", "Bearer " <> T.encodeUtf8 tok)] }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st   = statusCode (responseStatus resp)
                        body = responseBody resp
                    in if st == 200
                       then case eitherDecode body of
                                Left  e -> return $ Left (ParseError (T.pack e))
                                Right v -> return $ Right v
                       else return $ Left (HttpError st (decodeBody body))

-- | GET retournant une liste d'objets JSON (format NDJSON : un objet par ligne).
adictGetNDJSON :: FromJSON a => AdictSession -> String -> IO (Either AdictError [a])
adictGetNDJSON session apiPath = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { requestHeaders = [("Authorization", "Bearer " <> T.encodeUtf8 tok)] }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st   = statusCode (responseStatus resp)
                        body = responseBody resp
                    in if st == 200
                       then return $ parseNDJSON body
                       else return $ Left (HttpError st (decodeBody body))

-- | GET NDJSON retournant un 'ConduitT' qui émet chaque objet décodé.
--   Pratique pour traiter les résultats un par un sans les charger tous en liste.
adictGetNDJSONStream :: FromJSON a => AdictSession -> String -> IO (Either AdictError (ConduitT () a IO ()))
adictGetNDJSONStream session apiPath = do
    result <- adictGetNDJSON session apiPath
    case result of
        Left  e  -> return $ Left e
        Right vs -> return $ Right (yieldMany vs)

-- | GET retournant le corps brut de la réponse (pour débogage ou formats non JSON).
adictGetRaw :: AdictSession -> String -> IO (Either AdictError LBS.ByteString)
adictGetRaw session apiPath = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { requestHeaders = [("Authorization", "Bearer " <> T.encodeUtf8 tok)] }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp -> return $ Right (responseBody resp)

-- | PUT avec corps JSON.
adictPut :: (ToJSON req, FromJSON resp)
         => AdictSession -> String -> req -> IO (Either AdictError resp)
adictPut session apiPath body = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { method         = "PUT"
                    , requestBody    = RequestBodyLBS (encode body)
                    , requestHeaders =
                        [ ("Authorization", "Bearer " <> T.encodeUtf8 tok)
                        , ("Content-Type",  "application/json")
                        ]
                    }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st   = statusCode (responseStatus resp)
                        rb   = responseBody resp
                    in if st `elem` [200, 201]
                       then case eitherDecode rb of
                                Left  e -> return $ Left (ParseError (T.pack e))
                                Right v -> return $ Right v
                       else return $ Left (HttpError st (decodeBody rb))

-- | POST avec corps JSON.
adictPost :: (ToJSON req, FromJSON resp)
          => AdictSession -> String -> req -> IO (Either AdictError resp)
adictPost session apiPath body = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { method         = "POST"
                    , requestBody    = RequestBodyLBS (encode body)
                    , requestHeaders =
                        [ ("Authorization", "Bearer " <> T.encodeUtf8 tok)
                        , ("Content-Type",  "application/json")
                        ]
                    }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st   = statusCode (responseStatus resp)
                        rb   = responseBody resp
                    in if st == 200
                       then case eitherDecode rb of
                                Left  e -> return $ Left (ParseError (T.pack e))
                                Right v -> return $ Right v
                       else return $ Left (HttpError st (decodeBody rb))

-- | POST retournant NDJSON.
adictPostNDJSON :: FromJSON resp
               => AdictSession -> String -> Value -> IO (Either AdictError [resp])
adictPostNDJSON session apiPath body = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { method         = "POST"
                    , requestBody    = RequestBodyLBS (encode body)
                    , requestHeaders =
                        [ ("Authorization", "Bearer " <> T.encodeUtf8 tok)
                        , ("Content-Type",  "application/json")
                        ]
                    }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st   = statusCode (responseStatus resp)
                        rb   = responseBody resp
                    in if st == 200
                       then return $ parseNDJSON rb
                       else return $ Left (HttpError st (decodeBody rb))

-- | PATCH sans corps (pour révoquer un droit d'accès).
adictPatch :: FromJSON resp => AdictSession -> String -> IO (Either AdictError resp)
adictPatch session apiPath = do
    tokResult <- getBearerToken session
    case tokResult of
        Left  e   -> return $ Left e
        Right tok -> do
            let url = buildUrl (sessionConfig session) apiPath
            initReq <- parseRequest url
            let req = initReq
                    { method         = "PATCH"
                    , requestHeaders = [("Authorization", "Bearer " <> T.encodeUtf8 tok)]
                    }
            result <- try (httpLbs req (sessionManager session))
                        :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left  e    -> return $ Left (NetworkError (T.pack (show e)))
                Right resp ->
                    let st   = statusCode (responseStatus resp)
                        rb   = responseBody resp
                    in if st == 200
                       then case eitherDecode rb of
                                Left  e -> return $ Left (ParseError (T.pack e))
                                Right v -> return $ Right v
                       else return $ Left (HttpError st (decodeBody rb))


-- ---------------------------------------------------------------------------
-- Utilitaires

-- | Parse un corps NDJSON (une ligne = un objet JSON) en liste de valeurs Haskell.
parseNDJSON :: FromJSON a => LBS.ByteString -> Either AdictError [a]
parseNDJSON body =
    let ls      = filter (not . LBS.null) $ LBS.split 10 body  -- split sur '\n'
        ls'     = map stripCR ls
        results = map eitherDecode ls'
    in case sequence results of
           Left  e  -> Left (ParseError (T.pack e))
           Right vs -> Right vs

-- | Supprime le '\r' final si présent (ligne Windows CRLF).
stripCR :: LBS.ByteString -> LBS.ByteString
stripCR bs = case LBS.unsnoc bs of
    Just (bs', 13) -> bs'
    _              -> bs

-- | Décode un corps de réponse en Text (pour les messages d'erreur).
decodeBody :: LBS.ByteString -> Text
decodeBody = T.decodeUtf8 . LBS.toStrict
