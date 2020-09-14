{-# LANGUAGE DerivingStrategies #-}

module External.Delhivery.Flow where

import Beckn.Types.Common (FlowR)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import External.Delhivery.Types
import Servant (FormUrlEncoded, JSON, Post, ReqBody, (:>))
import Servant.Client (BaseUrl, ClientError)

type TokenAPI =
  "auth" :> "realms" :> "HLD-Test" :> "protocol" :> "openid-connect" :> "token"
    :> ReqBody '[FormUrlEncoded] TokenReq
    :> Post '[JSON] TokenRes

tokenAPI :: Proxy TokenAPI
tokenAPI = Proxy

getToken :: BaseUrl -> TokenReq -> FlowR e (Either ClientError TokenRes)
getToken url req = L.callAPI url tokenReq
  where
    tokenReq = T.client tokenAPI req
