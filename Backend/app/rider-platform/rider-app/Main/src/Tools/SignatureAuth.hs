{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.SignatureAuth where

import Control.Arrow
import Control.Lens ((.=), (?=))
import Control.Lens.At (at)
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.Store.X509 as CryptoStore
import qualified Data.Aeson as A
import Data.Aeson.Types ((.:))
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Lazy as BSL hiding (ByteString)
import Data.List (lookup)
import qualified Data.OpenApi as DS
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (typeRep)
import Data.X509 (PubKey (PubKeyRSA))
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (fromList, (.~))
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import Kernel.Types.Base64
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import Kernel.Utils.Servant.Server (HasEnvEntry (..), runFlowRDelayedIO)
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
import qualified Storage.CachedQueries.Merchant as QMerchant

-- | Adds authentication via a signature in the API
--
-- The lookup argument defines how keys can be looked up for performing
-- signature matches.
data SignatureAuth (req :: Type) (header :: Symbol)

newtype SignatureAuthResult req = SignatureAuthResult
  { requestBody :: req
  }

data SignatureAuthReq = SignatureAuthReq
  { merchantId :: ShortId Merchant,
    timestamp :: UTCTime
  }
  deriving (Generic, Show)

instance A.FromJSON SignatureAuthReq where
  parseJSON v = case v of
    A.Object obj ->
      SignatureAuthReq <$> obj .: "merchantId"
        <*> obj .: "timestamp"
    A.String s ->
      case A.eitherDecodeStrict (TE.encodeUtf8 s) of
        Left err -> fail err
        Right req -> return req
    _ -> fail "Invalid JSON format for SignatureAuthReq"

-- | This server part implementation accepts a signature in @header@ and
-- verifies it using registry
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    KnownSymbol header,
    FromJSON req,
    HasField "hedisEnv" r Redis.HedisEnv,
    HasField "hedisNonCriticalEnv" r Redis.HedisEnv,
    HasField "hedisNonCriticalClusterEnv" r Redis.HedisEnv,
    HasField "hedisClusterEnv" r Redis.HedisEnv,
    HasField "hedisMigrationStage" r Bool,
    HasField "esqDBEnv" r EsqDBEnv,
    HasField "enablePrometheusMetricLogging" r Bool,
    HasField "enableRedisLatencyLogging" r Bool,
    HasLog r,
    HasCoreMetrics r,
    HasCacheConfig r
  ) =>
  HasServer (SignatureAuth req header :> api) ctx
  where
  type
    ServerT (SignatureAuth req header :> api) m =
      SignatureAuthResult req -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Wai.Request -> DelayedIO (SignatureAuthResult req)
      authCheck req = runFlowRDelayedIO env . withLogTag "authCheck" $ do
        rawReq <- liftIO $ Wai.strictRequestBody req <&> BSL.toStrict
        signature <-
          Wai.requestHeaders req
            & (lookup headerName >>> fromMaybeM (MissingHeader headerName))
            >>= (parseHeader >>> fromEitherM (InvalidHeader headerName))
        signatureAuthReq :: SignatureAuthReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
        request :: req <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
        merchant <- QMerchant.findByShortId signatureAuthReq.merchantId >>= fromMaybeM (MerchantDoesNotExist signatureAuthReq.merchantId.getShortId)
        let nominal = realToFrac merchant.signatureExpiry
        expired <- isExpired nominal signatureAuthReq.timestamp
        let decodedSignature = B64.decodeLenient . TE.encodeUtf8 $ signature
            publicKeys = getRSAPublicKey merchant.signingPublicKey
        case publicKeys of
          [PubKeyRSA publicKey] -> do
            let isVerified = RSA.verify (Just Hash.SHA256) publicKey rawReq decodedSignature
            if isVerified && not expired
              then return $ SignatureAuthResult request
              else throwError Unauthorized
          _ -> throwError (InternalError "Could not find valid credentials for the merchant.")
      headerName = fromString $ symbolVal (Proxy @header)
      headerName :: IsString a => a
      env = getEnvEntry ctx
      getRSAPublicKey (Base64 key) = CryptoStore.readPubKeyFileFromMemory $ B64.decodeLenient key
  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

instance
  (HasClient m api, KnownSymbol header) =>
  HasClient m (SignatureAuth req header :> api)
  where
  type Client m (SignatureAuth req header :> api) = Client m api

  clientWithRoute mp _ = clientWithRoute mp (Proxy @api)

  hoistClientMonad mp _ = hoistClientMonad mp (Proxy @api)

instance
  ( S.HasOpenApi api,
    KnownSymbol header
  ) =>
  S.HasOpenApi (SignatureAuth req header :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & addSecurityRequirement "Looks up the given key Id in the Core registry."
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @SignatureAuthReq)

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
  SanitizedUrl (SignatureAuth r h :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
