{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.SignatureAuth where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Error
import Beckn.Types.Flow
import Beckn.Types.Id
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import qualified Beckn.Utils.Registry as Registry
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
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Servant
  ( FromHttpApiData (parseHeader),
    HasServer (..),
    type (:>),
  )
import Servant.Client (BaseUrl (baseUrlHost), HasClient (..))
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
data SignatureAuth (header :: Symbol) lookup

-- | How key lookup is performed
class LookupMethod lookup where
  -- | Lookup result, what is passed to the endpoint implementation. This can
  -- be used to attach any result from the lookup (such as the calling entity)
  -- if signature verification succeeds.
  type LookupResult lookup

  -- | Description of this lookup scheme as it appears in swagger.
  lookupDescription :: Text

class AuthenticatingEntity r where
  getSelfUrl :: r -> BaseUrl
  getRegistry :: r -> [Credential]
  getSigningKeys :: r -> [SigningKey]
  getSignatureExpiry :: r -> Seconds

class LookupMethod lookup => HasLookupAction lookup m where
  runLookup :: LookupAction lookup m

-- | Implementation of lookup.
type LookupAction lookup m =
  -- LookupMethod lookup =>

  -- | Look up the given keyId and return a result to pass to the request,
  -- and a key that can be used for signature verification
  HttpSig.SignaturePayload ->
  m (LookupResult lookup, HttpSig.PublicKey, BaseUrl)

data LookupRegistry r = LookupRegistry

instance LookupMethod (LookupRegistry (r :: Type)) where
  type LookupResult (LookupRegistry r) = r
  lookupDescription =
    "Looks up the given key Id in the Beckn registry."

lookupRegistryAction ::
  ( MonadReader r m,
    MonadThrow m,
    Log m,
    AuthenticatingEntity r
  ) =>
  (ShortId a -> m (Maybe a)) ->
  LookupAction (LookupRegistry a) m
lookupRegistryAction findOrgByShortId signaturePayload = do
  appEnv <- ask
  let selfUrl = getSelfUrl appEnv
  let registry = getRegistry appEnv
  logTagDebug "SignatureAuth" $ "Got Signature: " <> show signaturePayload
  let uniqueKeyId = signaturePayload.params.keyId.uniqueKeyId
  let mCred = Registry.lookupKey uniqueKeyId registry
  cred <- case mCred of
    Just c -> return c
    Nothing -> do
      logTagError "SignatureAuth" $ "Could not look up uniqueKeyId: " <> uniqueKeyId
      throwError Unauthorized
  org <-
    findOrgByShortId (ShortId $ cred.shortOrgId)
      >>= maybe (throwError OrgNotFound) pure
  pk <- case Registry.decodeKey $ cred.signPubKey of
    Nothing -> do
      logTagError "SignatureAuth" $ "Invalid public key: " <> show (cred.signPubKey)
      throwError $ InternalError "Invalid public key."
    Just key -> return key
  return (org, pk, selfUrl)

data SignatureAuthResult res = SignatureAuthResult
  { signature :: HttpSig.SignaturePayload,
    lookupResult :: res
  }

-- | This server part implementation accepts a signature in @header@ and
-- verifies it using @LookupAction@.
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    KnownSymbol header,
    HasCoreMetrics r,
    HasLookupAction lookup (FlowR r)
  ) =>
  HasServer (SignatureAuth header lookup :> api) ctx
  where
  type
    ServerT (SignatureAuth header lookup :> api) m =
      SignatureAuthResult (LookupResult lookup) -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Wai.Request -> DelayedIO (SignatureAuthResult (LookupResult lookup))
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
        lookupResult <- verifySignature @lookup headerName signPayload bodyHash
        return $ SignatureAuthResult signPayload lookupResult
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
-- `signatureAuthManager` as their `Manager` to create and add the signature.
-- This is a bit ugly, but it does not appear that we have much of a choice in
-- this regard given what plumbing options Servant gives us.
instance
  (HasClient m api, KnownSymbol header) =>
  HasClient m (SignatureAuth header lookup :> api)
  where
  type Client m (SignatureAuth header lookup :> api) = Client m api

  clientWithRoute mp _ req =
    clientWithRoute
      mp
      (Proxy @api)
      req

  hoistClientMonad mp _ hst cli = hoistClientMonad mp (Proxy @api) hst cli

signatureAuthManagerKey :: String
signatureAuthManagerKey = "http-signature"

registryAuthManagerKey :: String
registryAuthManagerKey = "registry-manager"

signatureAuthManager ::
  R.FlowRuntime ->
  r ->
  Text ->
  Seconds ->
  Text ->
  HttpSig.PrivateKey ->
  Text ->
  Http.ManagerSettings
signatureAuthManager flowRt appEnv shortOrgId signatureExpiry header key uniqueKeyId =
  Http.tlsManagerSettings {Http.managerModifyRequest = runFlowR flowRt appEnv . doSignature}
  where
    doSignature req = do
      now <- liftIO getPOSIXTime
      let params = HttpSig.mkSignatureParams shortOrgId uniqueKeyId now signatureExpiry HttpSig.Ed25519
      body <- getBody $ Http.requestBody req
      let bodyHash = HttpSig.becknSignatureHash body
      let headers = Http.requestHeaders req
      let signatureMsg = HttpSig.makeSignatureString params bodyHash headers
      logTagDebug "signatureAuthManager" $ "Request body for signing: " +|| body ||+ ""
      logTagDebug "signatureAuthManager" $ "Signature Message: " +|| signatureMsg ||+ ""
      addSignature bodyHash params headers req
        & fromMaybeM (InternalError $ "Could not add signature: " <> show params)
    getBody (Http.RequestBodyLBS body) = pure $ BSL.toStrict body
    getBody (Http.RequestBodyBS body) = pure body
    getBody _ = pure "<MISSING_BODY>"

    -- FIXME: we don't currently deal with Content-Length not being there (this is
    -- filled later, so we might need to have some special handling)
    addSignature :: HttpSig.Hash -> HttpSig.SignatureParams -> Http.RequestHeaders -> Http.Request -> Maybe Http.Request
    addSignature bodyHash params headers req =
      let ciHeader = CI.mk $ encodeUtf8 header
       in -- We check if the header exists because `managerModifyRequest` might be
          -- called multiple times, so we already added it once, let's skip right over
          if isJust $ lookup ciHeader headers
            then Just req
            else do
              signature <- HttpSig.sign key params bodyHash headers
              let headerVal = HttpSig.encode $ HttpSig.SignaturePayload signature params
              Just $ req {Http.requestHeaders = (ciHeader, headerVal) : headers}

verifySignature ::
  forall lookup r m.
  (MonadReader r m, MonadThrow m, Log m, HasLookupAction lookup m) =>
  Text ->
  HttpSig.SignaturePayload ->
  HttpSig.Hash ->
  m (LookupResult lookup)
verifySignature headerName signPayload bodyHash = do
  (lookupResult, key, selfUrl) <- runLookup @lookup signPayload
  let host = fromString $ baseUrlHost selfUrl
  isVerified <- performVerification key host
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
    performVerification key host = do
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
      either (throwVerificationFail host) pure $
        HttpSig.verify
          key
          signatureParams
          bodyHash
          headers
          signature
    throwVerificationFail host err = do
      logTagError logTag $ "Failed to verify the signature. Error: " <> show err
      throwError $ SignatureVerificationFailure [HttpSig.mkSignatureRealm headerName host]

prepareAuthManager ::
  AuthenticatingEntity r =>
  R.FlowRuntime ->
  r ->
  Text ->
  Text ->
  Either Text Http.ManagerSettings
prepareAuthManager flowRt appEnv header shortOrgId = do
  let registry = getRegistry appEnv
  let signingKeys = getSigningKeys appEnv
  let mCred = Registry.lookupOrg shortOrgId registry
  let signatureExpiry = getSignatureExpiry appEnv
  creds <- mCred & maybeToRight ("No credentials for: " <> shortOrgId)
  let uniqueKeyId = creds.uniqueKeyId
  let mSigningKey = Registry.lookupSigningKey uniqueKeyId signingKeys
  encodedKey <- mSigningKey & maybeToRight ("No private key found for credential: " <> uniqueKeyId)
  let mPrivateKey = Registry.decodeKey $ encodedKey.signPrivKey
  privateKey <- mPrivateKey & maybeToRight ("Could not decode private key for credential: " <> uniqueKeyId)
  pure $ signatureAuthManager flowRt appEnv shortOrgId signatureExpiry header privateKey uniqueKeyId

makeManagerMap :: [String] -> [Http.ManagerSettings] -> Map String Http.ManagerSettings
makeManagerMap managerKeys managers = Map.fromList $ zip managerKeys managers

prepareAuthManagers ::
  AuthenticatingEntity r =>
  R.FlowRuntime ->
  r ->
  [Text] ->
  Either Text (Map String Http.ManagerSettings)
prepareAuthManagers flowRt appEnv allShortIds = do
  let managerKeys = map (\shortId -> signatureAuthManagerKey <> "-" <> T.unpack shortId) allShortIds
  mapM (prepareAuthManager flowRt appEnv "Authorization") allShortIds
    <&> makeManagerMap managerKeys

instance
  ( S.HasOpenApi api,
    LookupMethod lookup,
    Typeable lookup,
    KnownSymbol header
  ) =>
  S.HasOpenApi (SignatureAuth header lookup :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & addSecurityRequirement (lookupDescription @lookup)
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @lookup)

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
  SanitizedUrl (SignatureAuth h l :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
