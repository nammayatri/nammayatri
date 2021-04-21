{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.SignatureAuth where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Server (HasEnvEntry (..))
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Lens ((?=))
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Swagger as DS
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (typeRep)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Servant
  ( FromHttpApiData (parseHeader),
    HasServer (..),
    ServerError (errBody),
    err401,
    type (:>),
  )
import Servant.Client (BaseUrl (baseUrlHost), HasClient (..))
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal, withRequest)
import qualified Servant.Swagger as S
import qualified Servant.Swagger.Internal as S

-- | Adds authentication via a signature in the API
--
-- Follows the HTTP Signature specification at
-- https://tools.ietf.org/id/draft-cavage-http-signatures-12.html
--
-- The header is a parameter, so we can support both `Authorize` and
-- `Proxy-Authorize` variants.
--
-- The lookup argument defines how keys can be looked up for performing
-- signature matches.
data SignatureAuth (header :: Symbol)

-- | How key lookup is performed
class LookupMethod lookup where
  -- | Lookup result, what is passed to the endpoint implementation. This can
  -- be used to attach any result from the lookup (such as the calling entity)
  -- if signature verification succeeds.
  type LookupResult lookup

  -- | Description of this lookup scheme as it appears in swagger.
  lookupDescription :: Text

class AuthenticatingEntity r where
  getSelfId :: r -> Text
  getSelfUrl :: r -> BaseUrl
  getRegistry :: r -> [Credential]
  getSigningKeys :: r -> [SigningKey]
  getSignatureExpiry :: r -> NominalDiffTime

-- | Implementation of lookup.
data LookupAction lookup r = LookupMethod lookup =>
  LookupAction
  { -- | Look up the given keyId and return a result to pass to the request,
    -- and a key that can be used for signature verification
    runLookup :: HttpSig.SignaturePayload -> FlowR r (LookupResult lookup, HttpSig.PublicKey, BaseUrl)
  }

data LookupRegistry = LookupRegistry

instance LookupMethod LookupRegistry where
  type LookupResult LookupRegistry = Organization
  lookupDescription =
    "Looks up the given key Id in the Beckn registry."

lookupRegistryAction ::
  AuthenticatingEntity r =>
  (ShortId Organization -> FlowR r (Maybe Organization)) ->
  LookupAction LookupRegistry r
lookupRegistryAction findOrgByShortId = LookupAction $ \signaturePayload -> do
  appEnv <- ask
  let selfUrl = getSelfUrl appEnv
  let registry = getRegistry appEnv
  logTagDebug "SignatureAuth" $ "Got Signature: " <> show signaturePayload
  let uniqueKeyId = signaturePayload ^. #params . #keyId . #uniqueKeyId
  let mCred = Registry.lookupKey uniqueKeyId registry
  cred <- case mCred of
    Just c -> return c
    Nothing -> do
      logTagError "SignatureAuth" $ "Could not look up uniqueKeyId: " <> uniqueKeyId
      throwError Unauthorized
  org <-
    findOrgByShortId (ShortId $ cred ^. #shortOrgId)
      >>= maybe (throwError OrgNotFound) pure
  pk <- case Registry.decodeKey $ cred ^. #signPubKey of
    Nothing -> do
      logTagError "SignatureAuth" $ "Invalid public key: " <> show (cred ^. #signPubKey)
      throwError $ InternalError "Invalid public key."
    Just key -> return key
  return (org, pk, selfUrl)

-- | This server part implementation accepts a signature in @header@ and
-- verifies it using @LookupAction@.
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    KnownSymbol header
  ) =>
  HasServer (SignatureAuth header :> api) ctx
  where
  type
    ServerT (SignatureAuth header :> api) m =
      HttpSig.SignaturePayload -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Wai.Request -> DelayedIO HttpSig.SignaturePayload
      authCheck req = do
        let headers = Wai.requestHeaders req
        let headerName = fromString $ symbolVal (Proxy @header)
        let mSignature = snd <$> find ((== headerName) . fst) headers
        liftIO $ runFlowR flowRt (appEnv env) $ logTagDebug "authCheck" $ "Incoming headers: " +|| headers ||+ ""
        headerBs <-
          case mSignature of
            Just s -> pure s
            Nothing -> do
              let msg = fromString $ "Signature header " +|| headerName ||+ " missing"
              liftIO $ runFlowR flowRt appConfig $ logTagError "authCheck" $ decodeUtf8 msg
              delayedFailFatal err401 {errBody = msg}
        sigBs <-
          case fromString <$> parseHeader @String headerBs of
            Right s -> pure s
            Left err -> do
              let msg = fromString $ "Invalid signature header. Error: " +|| err ||+ ""
              liftIO $ runFlowR flowRt appConfig $ logTagError "authCheck" $ decodeUtf8 msg
              delayedFailFatal err401 {errBody = msg}
        case HttpSig.decode sigBs of
          Right res -> pure res
          Left err -> do
            let msg = fromString $ "Could not decode signature: " +|| err ||+ ""
            liftIO $ runFlowR flowRt appConfig $ logTagError "authCheck" $ decodeUtf8 msg
            delayedFailFatal err401 {errBody = msg}
      env = getEnvEntry ctx
      flowRt = flowRuntime env
      appConfig = appEnv env

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

-- | The client implementation for SignatureAuth is a no-op, as we do not have
-- a request that we can work with at this layer. Clients should instead use
-- `signatureAuthManager` as their `Manager` to create and add the signature.
-- This is a bit ugly, but it does not appear that we have much of a choice in
-- this regard given what plumbing options Servant gives us.
instance
  (HasClient m api, KnownSymbol header) =>
  HasClient m (SignatureAuth header :> api)
  where
  type Client m (SignatureAuth header :> api) = Client m api

  clientWithRoute mp _ req =
    clientWithRoute
      mp
      (Proxy @api)
      req

  hoistClientMonad mp _ hst cli = hoistClientMonad mp (Proxy @api) hst cli

signatureAuthManagerKey :: String
signatureAuthManagerKey = "http-signature"

signatureAuthManager ::
  R.FlowRuntime ->
  r ->
  Text ->
  NominalDiffTime ->
  Text ->
  HttpSig.PrivateKey ->
  Text ->
  IO Http.Manager
signatureAuthManager flowRt appEnv shortOrgId signatureExpiry header key uniqueKeyId = do
  Http.newManager
    Http.tlsManagerSettings {Http.managerModifyRequest = runFlowR flowRt appEnv . doSignature}
  where
    doSignature req = do
      now <- L.runIO getPOSIXTime
      let params = HttpSig.mkSignatureParams shortOrgId uniqueKeyId now signatureExpiry HttpSig.Ed25519
      body <- getBody $ Http.requestBody req
      let headers = Http.requestHeaders req
      let signatureMsg = HttpSig.makeSignatureString params body headers
      logTagDebug "signatureAuthManager" $ "Request body for signing: " +|| body ||+ ""
      logTagDebug "signatureAuthManager" $ "Signature Message: " +|| signatureMsg ||+ ""
      addSignature body params headers req
        & fromMaybeM (InternalError $ "Could not add signature: " <> show params)
    getBody (Http.RequestBodyLBS body) = pure $ BSL.toStrict body
    getBody (Http.RequestBodyBS body) = pure body
    getBody _ = pure "<MISSING_BODY>"

    -- FIXME: we don't currently deal with Content-Length not being there (this is
    -- filled later, so we might need to have some special handling)
    addSignature :: ByteString -> HttpSig.SignatureParams -> Http.RequestHeaders -> Http.Request -> Maybe Http.Request
    addSignature body params headers req =
      let ciHeader = CI.mk $ encodeUtf8 header
       in -- We check if the header exists because `managerModifyRequest` might be
          -- called multiple times, so we already added it once, let's skip right over
          if isJust $ lookup ciHeader headers
            then Just req
            else do
              signature <- HttpSig.sign key params body headers
              let headerVal = HttpSig.encode $ HttpSig.SignaturePayload signature params
              Just $ req {Http.requestHeaders = (ciHeader, headerVal) : headers}

verifySignature ::
  ToJSON body =>
  Text ->
  LookupAction lookup r ->
  HttpSig.SignaturePayload ->
  body ->
  FlowR r (LookupResult lookup)
verifySignature headerName (LookupAction runLookup) signPayload req = do
  (lookupResult, key, selfUrl) <- runLookup signPayload
  let host = fromString $ baseUrlHost selfUrl
  let body = BSL.toStrict . J.encode $ req -- TODO: we should be able to receive raw body without using Aeson encoders. Maybe use WAI middleware to catch a raw body before handling a req by Servant?
  isVerified <- performVerification key host body
  unless isVerified $ do
    logTagError logTag "Signature is not valid."
    throwError $ SignatureVerificationFailure [HttpSig.mkSignatureRealm getRealm host]
  pure lookupResult
  where
    logTag = "verifySignature-" <> headerName
    getRealm = case headerName of
      "Authorization" -> "WWW-Authenticate"
      "Proxy-Authorization" -> "Proxy-Authenticate"
      _ -> ""
    performVerification key host body = do
      let headers =
            [ ("(created)", maybe "" show (signPayload ^. #params . #created)),
              ("(expires)", maybe "" show (signPayload ^. #params . #expires)),
              ("digest", "")
            ]
      let signatureParams = signPayload ^. #params
      let signature = signPayload ^. #signature
      let signatureMsg = HttpSig.makeSignatureString signatureParams body headers
      logTagDebug logTag $
        "Start verifying. Signature: " +|| HttpSig.encode signPayload ||+ ", Signature Message: " +|| signatureMsg ||+ ", Body: " +|| body ||+ ""
      either (throwVerificationFail host) pure $
        HttpSig.verify
          key
          signatureParams
          body
          headers
          signature
    throwVerificationFail host err = do
      logTagError logTag $ "Failed to verify the signature. Error: " <> show err
      throwError $ SignatureVerificationFailure [HttpSig.mkSignatureRealm headerName host]

withBecknAuth ::
  ToJSON req =>
  (LookupResult lookup -> req -> FlowHandlerR r b) ->
  LookupAction lookup r ->
  HttpSig.SignaturePayload ->
  req ->
  FlowHandlerR r b
withBecknAuth handler lookupAction sign req = do
  lookupResult <- withUnblockableFlowHandlerAPI $ verifySignature "Authorization" lookupAction sign req
  handler lookupResult req

withBecknAuthProxy ::
  ToJSON req =>
  (LookupResult lookup -> req -> FlowHandlerR r b) ->
  LookupAction lookup r ->
  HttpSig.SignaturePayload ->
  HttpSig.SignaturePayload ->
  req ->
  FlowHandlerR r b
withBecknAuthProxy handler lookupAction sign proxySign req = do
  lookupResult <- withUnblockableFlowHandlerAPI $ verifySignature "Authorization" lookupAction sign req
  _ <- withUnblockableFlowHandlerAPI $ verifySignature "Proxy-Authorization" lookupAction proxySign req
  handler lookupResult req

prepareAuthManager ::
  AuthenticatingEntity r =>
  R.FlowRuntime ->
  r ->
  Text ->
  Text ->
  Either Text (IO Http.Manager)
prepareAuthManager flowRt appEnv header shortOrgId = do
  let registry = getRegistry appEnv
  let signingKeys = getSigningKeys appEnv
  let mCred = Registry.lookupOrg shortOrgId registry
  let signatureExpiry = getSignatureExpiry appEnv
  creds <- mCred & maybeToRight ("No credentials for: " <> shortOrgId)
  let uniqueKeyId = creds ^. #uniqueKeyId
  let mSigningKey = Registry.lookupSigningKey uniqueKeyId signingKeys
  encodedKey <- mSigningKey & maybeToRight ("No private key found for credential: " <> uniqueKeyId)
  let mPrivateKey = Registry.decodeKey $ encodedKey ^. #signPrivKey
  privateKey <- mPrivateKey & maybeToRight ("Could not decode private key for credential: " <> uniqueKeyId)
  pure $ signatureAuthManager flowRt appEnv shortOrgId signatureExpiry header privateKey uniqueKeyId

makeManagerMap :: [String] -> [Http.Manager] -> Map String Http.Manager
makeManagerMap managerKeys managers = Map.fromList $ zip managerKeys managers

prepareAuthManagers ::
  AuthenticatingEntity r =>
  R.FlowRuntime ->
  r ->
  [Text] ->
  Either Text (IO (Map String Http.Manager))
prepareAuthManagers flowRt appEnv allShortIds = do
  let managerKeys = map (\shortId -> signatureAuthManagerKey <> "-" <> T.unpack shortId) allShortIds
  let managerCreationActionEithers = map (prepareAuthManager flowRt appEnv "Authorization") allShortIds
  managerCreationActions <- sequence managerCreationActionEithers
  pure $ makeManagerMap managerKeys <$> sequence managerCreationActions

instance
  ( S.HasSwagger api,
    LookupMethod lookup,
    Typeable lookup,
    KnownSymbol header
  ) =>
  S.HasSwagger (SignatureAuth header :> api)
  where
  toSwagger _ =
    S.toSwagger (Proxy @api)
      & addSecurityRequirement (lookupDescription @lookup)
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @lookup)

      addSecurityRequirement :: Text -> DS.Swagger -> DS.Swagger
      addSecurityRequirement description = execState $ do
        DS.securityDefinitions . at methodName ?= securityScheme
        DS.allOperations . DS.security .= one securityRequirement
        where
          securityScheme =
            DS.SecurityScheme
              { _securitySchemeDescription = Just description,
                _securitySchemeType =
                  DS.SecuritySchemeApiKey
                    DS.ApiKeyParams
                      { _apiKeyName = headerName,
                        _apiKeyIn = DS.ApiKeyHeader
                      }
              }
          securityRequirement =
            let scopes = []
             in DS.SecurityRequirement $ fromList [(methodName, scopes)]

      addResponse401 :: DS.Swagger -> DS.Swagger
      addResponse401 = execState $ do
        DS.responses . at response401Name ?= response401
        DS.allOperations . DS.responses . DS.responses . at 401
          ?= DS.Ref (DS.Reference response401Name)
        where
          response401Name = "Unauthorized"
          response401 = mempty & DS.description .~ "Unauthorized"

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (SignatureAuth h :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
