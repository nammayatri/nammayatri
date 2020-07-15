{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.Auth where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Common
import Beckn.Utils.Servant.Server
import Control.Lens ((?=))
import Data.Kind (Type)
import qualified Data.Swagger as DS
import Data.Typeable (typeRep)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.Wai (Request (..))
import Servant
import Servant.Client
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal, withRequest)
import qualified Servant.Swagger as S
import qualified Servant.Swagger.Internal as S

-- | Adds authentication with token to API.
--
-- Type argument defines what verification logic is supposed to do.
-- Normally you should define a type alias for this which fixes the
-- verification method.
data TokenAuth' (header :: Symbol) (verify :: Type)

-- | How token verification is performed.
class VerificationMethod verify where
  -- | Verification result, what is passed to the endpoint implementation.
  type VerificationResult verify

  -- | Description of this verification scheme as it appears in swagger.
  verificationDescription :: Text

-- | Implementation of verification.
data VerificationAction verify r =
  VerificationMethod verify => VerificationAction
    { runVerifyMethod :: RegToken -> FlowR r (VerificationResult verify)
    }

-- | This server part implementation accepts token in @token@ header,
-- verifies it and puts @'VerificationResult'@ to your endpoint.
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    HasContextEntry ctx (VerificationAction verify r),
    VerificationMethod verify,
    KnownSymbol header
  ) =>
  HasServer (TokenAuth' header verify :> api) ctx
  where
  type
    ServerT (TokenAuth' header verify :> api) m =
      VerificationResult verify -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest (authCheck (runTime env))
    where
      authCheck :: R.FlowRuntime -> Request -> DelayedIO (VerificationResult verify)
      authCheck flowRt req = do
        let headerName = fromString $ symbolVal (Proxy @header)
        let mHeader = snd <$> find ((== headerName) . fst) (requestHeaders req)
        valBs <- maybe (formatErr $ "Header " <> show headerName <> " is required") pure mHeader
        val <-
          either (\_ -> formatErr "Invalid token header") pure $
            parseHeader @Text valBs
        -- If we don't use delayedFailFatal and just pass the exception,
        -- it will be JSON-formatted

        liftIO . runFlowR flowRt (appEnv env) $ verifyMethod val
      formatErr msg = delayedFailFatal err400 {errBody = msg}
      env = getEnvEntry ctx
      VerificationAction verifyMethod = getContextEntry ctx :: VerificationAction verify r

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

-- | This client part implementation simply accepts token and passes it to
-- the call.
instance
  (HasClient m api, KnownSymbol header) =>
  HasClient m (TokenAuth' header verify :> api)
  where
  type Client m (TokenAuth' header verify :> api) = RegToken -> Client m api

  clientWithRoute mp _ req =
    clientWithRoute
      mp
      (Proxy @(Header header Text :> api))
      req
      . Just

  hoistClientMonad mp _ hst cli = hoistClientMonad mp (Proxy @api) hst . cli

instance
  ( S.HasSwagger api,
    VerificationMethod verify,
    Typeable verify,
    KnownSymbol header
  ) =>
  S.HasSwagger (TokenAuth' header verify :> api)
  where
  toSwagger _ =
    S.toSwagger (Proxy @api)
      & addSecurityRequirement methodName (verificationDescription @verify) headerName
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @verify)

addSecurityRequirement :: Text -> Text -> Text -> DS.Swagger -> DS.Swagger
addSecurityRequirement methodName description headerName = execState $ do
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
