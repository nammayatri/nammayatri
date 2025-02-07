{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import Tools.Error

data AuthReq = AuthReq
  { username :: Text,
    password :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AuthRes = AuthRes
  { statusCode :: Int,
    message :: Text,
    result :: TokenResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype TokenResult = TokenResult
  { access_token :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type AuthAPI =
  "cumta" :> "authenticate"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.get authTokenKey
  case authToken of
    Nothing -> do
      password <- decrypt config.password
      auth <-
        callAPI config.networkHostUrl (ET.client authAPI $ AuthReq config.username password) "authCMRL" authAPI
          >>= fromEitherM (ExternalAPICallError (Just "CMRL_AUTH_API") config.networkHostUrl)
      Hedis.setExp authTokenKey auth.result.access_token (7 * 3600)
      return auth.result.access_token
    Just token -> return token
  where
    authTokenKey = "CMRLAuth:Token"
