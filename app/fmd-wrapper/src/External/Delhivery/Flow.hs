{-# LANGUAGE DerivingStrategies #-}

module External.Delhivery.Flow where

import Beckn.Types.Common (FlowR)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import External.Delhivery.Types
import Servant (FormUrlEncoded, Header, JSON, Post, ReqBody, (:>))
import Servant.Client (BaseUrl, ClientError)
import Types.Common

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

type QuoteAPI =
  "api" :> "mileage"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] QuoteReq
    :> Post '[JSON] QuoteRes

quoteAPI :: Proxy QuoteAPI
quoteAPI = Proxy

getQuote :: Token -> BaseUrl -> QuoteReq -> FlowR e (Either ClientError QuoteRes)
getQuote token url req = L.callAPI url quoteReq
  where
    quoteReq = T.client quoteAPI (Just token) req

type CreateOrderAPI =
  "api" :> "createOrderRequest"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] CreateOrderReq
    :> Post '[JSON] CreateOrderRes

createOrderAPI :: Proxy CreateOrderAPI
createOrderAPI = Proxy

createOrder :: Token -> BaseUrl -> CreateOrderReq -> FlowR e (Either ClientError CreateOrderRes)
createOrder token url req = L.callAPI url createOrderReq
  where
    createOrderReq = T.client createOrderAPI (Just token) req
