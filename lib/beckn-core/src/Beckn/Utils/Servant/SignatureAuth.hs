{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.SignatureAuth where

import Beckn.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Error
import Beckn.Types.Flow
import Beckn.Types.Registry
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging (HasLog)
import Beckn.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import Beckn.Utils.Servant.Server (HasEnvEntry (..), runFlowRDelayedIO)
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Arrow
import Control.Lens ((?=))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as DS
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (typeRep)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Wai as Wai
import Servant
  ( FromHttpApiData (parseHeader),
    HasServer (..),
    type (:>),
  )
import Servant.Client (HasClient (..))
import qualified Servant.OpenApi as S
import qualified Servant.OpenApi.Internal as S
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, withRequest)

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

class AuthenticatingEntity r where
  getSigningKey :: r -> PrivateKey
  getSignatureExpiry :: r -> Seconds

data AuthenticatingEntity' = AuthenticatingEntity'
  { signingKey :: PrivateKey,
    uniqueKeyId :: Text,
    signatureExpiry :: Seconds
  }
  deriving (Generic, FromDhall)

data SignatureAuthResult = SignatureAuthResult
  { signature :: HttpSig.SignaturePayload,
    subscriber :: Subscriber
  }

-- | This server part implementation accepts a signature in @header@ and
-- verifies it using registry
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    KnownSymbol header,
    HasLog r,
    HasField "hostName" r Text,
    HasField "disableSignatureAuth" r Bool,
    Registry (FlowR r),
    HasCoreMetrics r
  ) =>
  HasServer (SignatureAuth header :> api) ctx
  where
  type
    ServerT (SignatureAuth header :> api) m =
      SignatureAuthResult -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Wai.Request -> DelayedIO SignatureAuthResult
      authCheck req = runFlowRDelayedIO env . becknApiHandler . withLogTag "authCheck" $ do
        let headers = Wai.requestHeaders req
        logDebug $ "Incoming headers: " +|| headers ||+ ""
        bodyHash <-
          headers
            & (lookup HttpSig.bodyHashHeader >>> fromMaybeM missingHashHeader)
            >>= (Base64.decodeLenient >>> HttpSig.hashFromByteString >>> fromMaybeM invalidHashHeader)
        signPayload <-
          headers
            & (lookup headerName >>> fromMaybeM (MissingHeader headerName))
            >>= (parseHeader >>> fromEitherM (InvalidHeader headerName))
            >>= (HttpSig.decode . fromString >>> fromEitherM CannotDecodeSignature)
        subscriber <- verifySignature headerName signPayload bodyHash
        return $ SignatureAuthResult signPayload subscriber
      headerName = fromString $ symbolVal (Proxy @header)
      headerName :: IsString a => a
      -- These are 500 because we must add that header in wai middleware
      missingHashHeader = InternalError $ "Header " +|| HttpSig.bodyHashHeader ||+ " not found"
      invalidHashHeader = InternalError $ "Header " +|| HttpSig.bodyHashHeader ||+ " does not contain a valid hash"
      env = getEnvEntry ctx

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

-- | The client implementation for SignatureAuth is a no-op, as we do not have
-- a request that we can work with at this layer. Clients should instead use
-- `prepareAuthManager` as their `Manager` to create and add the signature.
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

prepareAuthManager ::
  HasLog r =>
  AuthenticatingEntity r =>
  R.FlowRuntime ->
  r ->
  [Text] ->
  Text ->
  Text ->
  Http.ManagerSettings
prepareAuthManager flowRt appEnv signHeaders subscriberId uniqueKeyId =
  Http.tlsManagerSettings {Http.managerModifyRequest = runFlowR flowRt appEnv . doSignature}
  where
    doSignature req = withLogTag "prepareAuthManager" do
      now <- liftIO getPOSIXTime
      let params = HttpSig.mkSignatureParams subscriberId uniqueKeyId now signatureExpiry HttpSig.Ed25519
      let body = getBody $ Http.requestBody req
      let bodyHash = HttpSig.becknSignatureHash body
      let headers = Http.requestHeaders req
      let signatureMsg = HttpSig.makeSignatureString params bodyHash headers
      logDebug $ "Request body for signing: " +|| body ||+ ""
      logDebug $ "Signature Message: " +|| signatureMsg ||+ ""
      foldM (addSignature bodyHash params headers) req signHeaders
        & fromMaybeM (InternalError $ "Could not add signature: " <> show params)
    getBody (Http.RequestBodyLBS body) = BSL.toStrict body
    getBody (Http.RequestBodyBS body) = body
    getBody _ = "<MISSING_BODY>"
    signPrivKey = getSigningKey appEnv
    signatureExpiry = getSignatureExpiry appEnv

    -- FIXME: we don't currently deal with Content-Length not being there (this is
    -- filled later, so we might need to have some special handling)
    addSignature bodyHash params headers req signHeader =
      let ciHeader = CI.mk $ encodeUtf8 signHeader
       in -- We check if the signHeader exists because `managerModifyRequest` might be
          -- called multiple times, so we already added it once, let's skip right over
          if isJust $ lookup ciHeader headers
            then Just req
            else do
              signature <- HttpSig.sign signPrivKey params bodyHash headers
              let headerVal = HttpSig.encode $ HttpSig.SignaturePayload signature params
              Just $ req {Http.requestHeaders = (ciHeader, headerVal) : headers}

verifySignature ::
  ( MonadFlow m,
    MonadReader r m,
    Metrics.CoreMetrics m,
    HasField "hostName" r Text,
    HasField "disableSignatureAuth" r Bool,
    Registry m,
    HasLog r
  ) =>
  Text ->
  HttpSig.SignaturePayload ->
  HttpSig.Hash ->
  m Subscriber
verifySignature headerName signPayload bodyHash = do
  hostName <- asks (.hostName)
  logTagDebug "SignatureAuth" $ "Got Signature: " <> show signPayload
  let uniqueKeyId = signPayload.params.keyId.uniqueKeyId
  let subscriberId = signPayload.params.keyId.subscriberId
      lookupRequest =
        SimpleLookupRequest
          { unique_key_id = uniqueKeyId,
            subscriber_id = subscriberId
          }
  registryLookup lookupRequest >>= \case
    Just subscriber -> do
      disableSignatureAuth <- asks (.disableSignatureAuth)
      unless disableSignatureAuth do
        let publicKey = subscriber.signing_public_key
        isVerified <- performVerification publicKey hostName
        unless isVerified $ do
          logTagError logTag "Signature is not valid."
          throwError $ getSignatureError hostName
      pure subscriber
    Nothing -> do
      logTagError logTag $
        "Subscriber with unique_key_id "
          <> signPayload.params.keyId.uniqueKeyId
          <> " not found."
      throwError $ getSignatureError hostName
  where
    logTag = "verifySignature-" <> headerName
    performVerification key hostName = do
      let headers =
            [ ("(created)", maybe "" show (signPayload.params.created)),
              ("(expires)", maybe "" show (signPayload.params.expires)),
              ("digest", "")
            ]
      let signatureParams = signPayload.params
      let signature = signPayload.signature
      let signatureMsg = HttpSig.makeSignatureString signatureParams bodyHash headers
      logTagDebug logTag $
        "Start verifying. Signature: " +|| HttpSig.encode signPayload ||+ ", Signature Message: " +|| signatureMsg ||+ ", Body hash: " +|| bodyHash ||+ ""
      let verificationResult =
            HttpSig.verify
              key
              signatureParams
              bodyHash
              headers
              signature
      case verificationResult of
        Right result -> pure result
        Left err -> do
          logTagError logTag $ "Failed to verify the signature. Error: " <> show err
          throwError $ getSignatureError hostName

    getSignatureError hostName =
      SignatureVerificationFailure [HttpSig.mkSignatureRealm getRealm hostName]

    getRealm = case headerName of
      "Authorization" -> "WWW-Authenticate"
      "Proxy-Authorization" -> "Proxy-Authenticate"
      "X-Gateway-Authorization" -> "Proxy-Authenticate"
      _ -> ""

prepareAuthManagers ::
  (AuthenticatingEntity r, HasLog r) =>
  R.FlowRuntime ->
  r ->
  [(Text, Text)] ->
  Map String Http.ManagerSettings
prepareAuthManagers flowRt appEnv allShortIds = do
  flip foldMap allShortIds \(shortId, uniqueKeyId) ->
    Map.singleton
      (signatureAuthManagerKey <> "-" <> T.unpack shortId)
      (prepareAuthManager flowRt appEnv ["Authorization"] shortId uniqueKeyId)

modFlowRtWithAuthManagers ::
  ( AuthenticatingEntity r,
    HasHttpClientOptions r c,
    MonadReader r m,
    HasLog r,
    MonadFlow m
  ) =>
  R.FlowRuntime ->
  r ->
  [(Text, Text)] ->
  m R.FlowRuntime
modFlowRtWithAuthManagers flowRt appEnv orgShortIds = do
  let managersSettings = prepareAuthManagers flowRt appEnv orgShortIds
  managers <- createManagers managersSettings
  logInfo $ "Loaded http managers - " <> show orgShortIds
  pure $ flowRt {R._httpClientManagers = managers}

instance
  ( S.HasOpenApi api,
    KnownSymbol header
  ) =>
  S.HasOpenApi (SignatureAuth header :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & addSecurityRequirement "Looks up the given key Id in the Beckn registry."
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @Subscriber)

      addSecurityRequirement :: Text -> DS.OpenApi -> DS.OpenApi
      addSecurityRequirement description = execState $ do
        DS.components . DS.securitySchemes . at methodName ?= securityScheme
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

      addResponse401 :: DS.OpenApi -> DS.OpenApi
      addResponse401 = execState $ do
        DS.components . DS.responses . at response401Name ?= response401
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
