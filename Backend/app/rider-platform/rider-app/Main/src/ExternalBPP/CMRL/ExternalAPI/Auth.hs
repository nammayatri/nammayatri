{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.CMRL.ExternalAPI.Auth where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Kernel.Utils.JSON
import Servant
import Tools.Error
import Web.FormUrlEncoded (ToForm, toForm)

data AuthReq = AuthReq
  { username :: Text,
    password :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON AuthReq where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON AuthReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

data AuthRes = AuthRes
  { accessToken :: Text
  }
  deriving (Generic, Show)

instance ToJSON AuthRes where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON AuthRes where
  parseJSON = genericParseJSON constructorsWithSnakeCase

type AuthAPI =
  "authenticate"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r) => CMRLConfig -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.get authTokenKey
  case authToken of
    Nothing -> do
      auth <-
        callAPI config.networkHostUrl (ET.client authAPI $ AuthReq config.username config.password) "authCMRL" authAPI
          >>= fromEitherM (ExternalAPICallError (Just "CMRL_AUTH_API") config.networkHostUrl)
      Hedis.setExp authTokenKey auth.accessToken (8 * 3600)
      return auth.accessToken
    Just token -> return token
  where
    authTokenKey = "CMRLAuth:Token"
