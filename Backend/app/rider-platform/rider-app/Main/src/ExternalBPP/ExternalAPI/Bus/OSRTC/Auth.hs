module ExternalBPP.ExternalAPI.Bus.OSRTC.Auth where

import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.OSRTC.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant hiding (throwError)
import Tools.Error

type GenerateAccessAPI =
  "api"
    :> "Authentication"
    :> "GenerateAccess"
    :> ReqBody '[JSON] OSRTCGenerateAccessReq
    :> Post '[JSON] (OSRTCResponse OSRTCGenerateAccessResList)

generateAccessAPI :: Proxy GenerateAccessAPI
generateAccessAPI = Proxy

getAuthToken :: forall m r. (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => OSRTCConfig -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.get (authTokenKey config)
  case authToken of
    Nothing -> fetchAndCacheToken config
    Just token -> return token
  where
    authTokenKey cfg = "OSRTCAuth:Token:" <> cfg.userName <> ":" <> show cfg.platformId

    fetchAndCacheToken :: OSRTCConfig -> m Text
    fetchAndCacheToken cfg = do
      decryptedKey <- decrypt cfg.secretKey
      let req = OSRTCGenerateAccessReq {userName = cfg.userName, secretKey = decryptedKey}
      resp <-
        callAPI cfg.baseUrl (ET.client generateAccessAPI req) "OSRTC:GenerateAccess" generateAccessAPI
          >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GENERATE_ACCESS_API") cfg.baseUrl)
      case resp._data of
        (accessRes : _) -> do
          now <- getCurrentTime
          let expiresAt = accessRes.dteAccessTokenExpirationTime
              authTokenTtl = max 1 . floor $ diffUTCTime expiresAt now - 60
          Hedis.setExp (authTokenKey cfg) accessRes.strAccessToken authTokenTtl
          return accessRes.strAccessToken
        [] -> throwError $ InternalError "OSRTC GenerateAccess returned empty data"
