{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.SignatureAuth where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import Beckn.Utils.Servant.Server
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Lens ((?=))
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Swagger as DS
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (typeRep)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Wai as Wai (Request (..))
import Servant
import Servant.Client
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
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
data SignatureAuth (header :: Symbol) (lookup :: Type)

-- | How key lookup is performed
class LookupMethod lookup where
  -- | Lookup result, what is passed to the endpoint implementation. This can
  -- be used to attach any result from the lookup (such as the calling entity)
  -- if signature verification succeeds.
  type LookupResult lookup

  -- | Description of this lookup scheme as it appears in swagger.
  lookupDescription :: Text

-- | Implementation of lookup.
data LookupAction lookup r = LookupMethod lookup =>
  LookupAction
  { -- | Look up the given keyId and return a result to pass to the request,
    -- and a key that can be used for signature verification
    runLookup :: Text -> FlowR r (LookupResult lookup, ByteString)
  }

-- | This server part implementation accepts a signature in @header@ and
-- verifies it using @LookupAction@.
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    HasContextEntry ctx (LookupAction lookup r),
    LookupMethod lookup,
    KnownSymbol header
  ) =>
  HasServer (SignatureAuth header lookup :> api) ctx
  where
  type
    ServerT (SignatureAuth header lookup :> api) m =
      LookupResult lookup -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest (authCheck (runTime env))
    where
      authCheck :: R.FlowRuntime -> Wai.Request -> DelayedIO (LookupResult lookup)
      authCheck flowRt req = do
        let method = Wai.requestMethod req
        let path = Wai.rawPathInfo req
        let headers = Wai.requestHeaders req
        let headerName = fromString $ symbolVal (Proxy @header)
        let mSignature = snd <$> find ((== headerName) . fst) headers
        -- FIXME: we 404 for now to allow a fallback to X-API-Key
        headerBs <-
          maybe
            (fail404 $ "Signature header " <> show headerName <> " missing")
            pure
            mSignature
        sigBs <-
          either (const $ fail401 "Invalid signature header") pure $
            fromString <$> parseHeader @String headerBs
        (sig, params) <-
          either (\e -> fail401 . fromString $ "Could not decode signature: " <> e) pure $
            HttpSig.decode sigBs
        -- If we don't use delayedFailFatal and just pass the exception,
        -- it will be JSON-formatted
        (result, key) <- liftIO . runFlowR flowRt (appEnv env) $ runLookup $ HttpSig.keyId params
        unless
          (HttpSig.verify key params method path headers sig)
          (fail401 $ "Signature verification failed on " <> show headerName <> " header")
        return result

      fail404 msg = delayedFail err404 {errBody = msg}
      fail401 msg = delayedFailFatal err401 {errBody = msg}
      env = getEnvEntry ctx
      LookupAction runLookup = getContextEntry ctx :: LookupAction lookup r

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

signatureAuthManager :: MonadIO m => HttpSig.PrivateKey -> Text -> NominalDiffTime -> m Http.Manager
signatureAuthManager key keyId validity = do
  liftIO $
    Http.newManager
      Http.tlsManagerSettings {Http.managerModifyRequest = doSignature}
  where
    doSignature req = do
      now <- getPOSIXTime
      let params =
            HttpSig.SignatureParams
              keyId
              HttpSig.Hs2019
              ["(request-target)"]
              (Just now)
              (Just $ now + validity)
      -- FIXME: can we throw insteado fromJust here?
      return $ fromJust $ addSignature params "Authorization" req

    -- FIXME: we don't currently deal with Content-Length not being there (this is
    -- filled later, so we might need to have some special handling)
    addSignature :: HttpSig.SignatureParams -> Text -> Http.Request -> Maybe Http.Request
    addSignature params header req =
      let headers = Http.requestHeaders req
          ciHeader = CI.mk $ encodeUtf8 header
       in -- We check if the header exists because `managerModifyRequest` might be
          -- called multiple times, so we already added it once, let's skip right over
          if isJust $ lookup ciHeader headers
            then Just req
            else do
              let method = Http.method req
                  path = Http.path req
              signature <- HttpSig.sign key params method path headers
              let headerVal = HttpSig.encode signature params
              Just $ req {Http.requestHeaders = (ciHeader, headerVal) : headers}

instance
  ( S.HasSwagger api,
    LookupMethod lookup,
    Typeable lookup,
    KnownSymbol header
  ) =>
  S.HasSwagger (SignatureAuth header lookup :> api)
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
  SanitizedUrl (SignatureAuth h l :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
