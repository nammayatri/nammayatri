{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Servant.Auth
  ( TokenAuth',
    VerificationMethod (..),
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Common
import Control.Lens ((?=))
import qualified Data.Swagger as DS
import Data.Typeable (typeRep)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.Exts (fromList)
import GHC.TypeLits (symbolVal)
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
data TokenAuth' verify

-- Name of header we expect in auth

type TokenHeaderName = "token"

tokenHeaderName :: IsString s => s
tokenHeaderName = fromString $ symbolVal (Proxy @TokenHeaderName)

-- | How token verification is performed.
class VerificationMethod verify where
  -- | Verification result, what is passed to the endpoint implementation.
  type VerificationResult verify

  -- | Verification logic.
  verifyToken :: RegToken -> FlowR () (VerificationResult verify)

  -- | Description of this verification scheme as it appears in swagger.
  verificationDescription :: Text

-- | This server part implementation accepts token in @token@ header,
-- verifies it and puts @'VerificationResult'@ to your endpoint.
instance
  (HasServer api ctx, HasContextEntry ctx R.FlowRuntime, VerificationMethod verify) =>
  HasServer (TokenAuth' verify :> api) ctx
  where
  type
    ServerT (TokenAuth' verify :> api) m =
      VerificationResult verify -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest (authCheck (getContextEntry ctx))
    where
      authCheck :: R.FlowRuntime -> Request -> DelayedIO (VerificationResult verify)
      authCheck flowRt req = do
        let mHeader = snd <$> find ((== tokenHeaderName) . fst) (requestHeaders req)

        valBs <- maybe (formatErr "Header 'token' is required") pure mHeader

        val <-
          either (\_ -> formatErr "Invalid token header") pure $
            parseHeader @Text valBs

        -- If we don't use delayedFailFatal and just pass the exception,
        -- it will be JSON-formatted

        liftIO . runFlowR flowRt () $ verifyToken @verify val
      formatErr msg = delayedFailFatal err400 {errBody = msg}

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

-- | This client part implementation simply accepts token and passes it to
-- the call.
instance HasClient m api => HasClient m (TokenAuth' verify :> api) where
  type Client m (TokenAuth' verify :> api) = RegToken -> Client m api

  clientWithRoute mp _ req =
    clientWithRoute
      mp
      (Proxy @(Header TokenHeaderName Text :> api))
      req
      . Just

  hoistClientMonad mp _ hst cli = hoistClientMonad mp (Proxy @api) hst . cli

instance
  (S.HasSwagger api, VerificationMethod verify, Typeable verify) =>
  S.HasSwagger (TokenAuth' verify :> api)
  where
  toSwagger _ =
    S.toSwagger (Proxy @api)
      & addSecurityRequirement methodName (verificationDescription @verify)
      & S.addDefaultResponse400 tokenHeaderName
      & addResponse401
    where
      methodName = show $ typeRep (Proxy @verify)

addSecurityRequirement :: Text -> Text -> DS.Swagger -> DS.Swagger
addSecurityRequirement methodName description = execState $ do
  DS.securityDefinitions . at methodName ?= securityScheme

  DS.allOperations . DS.security .= one securityRequirement
  where
    securityScheme =
      DS.SecurityScheme
        { _securitySchemeDescription = Just description,
          _securitySchemeType =
            DS.SecuritySchemeApiKey
              DS.ApiKeyParams
                { _apiKeyName = tokenHeaderName,
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
