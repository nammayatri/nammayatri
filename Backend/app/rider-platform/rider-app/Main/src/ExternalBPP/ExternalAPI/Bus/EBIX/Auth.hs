{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Bus.EBIX.Auth where

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

instance ToForm AuthReq where
  toForm AuthReq {..} =
    [ ("username", toQueryParam username),
      ("password", toQueryParam password),
      ("grant_type", toQueryParam $ T.pack "password")
    ]

data AuthRes = AuthRes
  { accessToken :: Text,
    expiresIn :: Seconds
  }
  deriving (Generic)

instance ToJSON AuthRes where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON AuthRes where
  parseJSON = genericParseJSON constructorsWithSnakeCase

type AuthAPI =
  "token"
    :> ReqBody '[FormUrlEncoded] AuthReq
    :> Get '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => EBIXConfig -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.get authTokenKey
  case authToken of
    Nothing -> do
      password <- decrypt config.password
      auth <-
        callAPI config.networkHostUrl (ET.client authAPI $ AuthReq config.username password) "authEBIX" authAPI
          >>= fromEitherM (ExternalAPICallError (Just "EBIX_AUTH_API") config.networkHostUrl)
      Hedis.setExp authTokenKey auth.accessToken auth.expiresIn.getSeconds
      return auth.accessToken
    Just token -> return token
  where
    authTokenKey = "EbixAuth:Token"
