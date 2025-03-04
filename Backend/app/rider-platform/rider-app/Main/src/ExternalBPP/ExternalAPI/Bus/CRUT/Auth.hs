{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Bus.CRUT.Auth where

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

instance ToForm AuthReq where -- makes AuthReq convertible to application/x-www-form-urlencoded format (needed for API requests).
  toForm AuthReq {..} =
    [ ("username", toQueryParam username),
      ("password", toQueryParam password)
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
  "auth"
    :> Header "externalauth" Text
    :> ReqBody '[FormUrlEncoded] AuthReq
    :> Post '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => EBIXConfig -> Text -> m Text
getAuthToken config externalAuthToken = do
  authToken :: (Maybe Text) <- Hedis.get authTokenKey
  case authToken of
    Nothing -> do
      password <- decrypt config.password -- Decrypts the password before making the request.
      auth <-
        callAPI config.networkHostUrl
          [("externalauth", externalAuthToken)]
          (ET.client authAPI (Just externalAuthToken) (AuthReq config.username password))
          "authCRUT"
          authAPI
          >>= fromEitherM (ExternalAPICallError (Just "CRUT_AUTH_API") config.networkHostUrl)
      Hedis.setExp authTokenKey auth.accessToken auth.expiresIn.getSeconds -- Stores the access token in Redis, Sets an expiration time based on expiresIn
      return auth.accessToken
    Just token -> return token
  where
    authTokenKey = "CRUTAuth:Token"
