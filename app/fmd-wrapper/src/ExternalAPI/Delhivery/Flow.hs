{-# LANGUAGE DerivingStrategies #-}

module ExternalAPI.Delhivery.Flow where

import Beckn.Types.Common (FlowR)
import Beckn.Utils.Common (callAPI)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import ExternalAPI.Delhivery.Types
import Servant (FormUrlEncoded, Header, JSON, Post, ReqBody, (:>))
import Servant.Client (BaseUrl, ClientError)
import Types.Common
import Types.Metrics

type TokenAPI =
  "auth" :> "realms" :> "HLD-Test" :> "protocol" :> "openid-connect" :> "token"
    :> ReqBody '[FormUrlEncoded] TokenReq
    :> Post '[JSON] TokenRes

tokenAPI :: Proxy TokenAPI
tokenAPI = Proxy

getToken :: HasCoreMetrics r => BaseUrl -> TokenReq -> FlowR r (Either ClientError TokenRes)
getToken url req = callAPI url tokenReq "getToken"
  where
    tokenReq = T.client tokenAPI req

type QuoteAPI =
  "api" :> "mileage"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] QuoteReq
    :> Post '[JSON] QuoteRes

quoteAPI :: Proxy QuoteAPI
quoteAPI = Proxy

getQuote :: HasCoreMetrics r => Token -> BaseUrl -> QuoteReq -> FlowR r (Either ClientError QuoteRes)
getQuote token url req = callAPI url quoteReq "getQuote"
  where
    quoteReq = T.client quoteAPI (Just token) req

type CreateOrderAPI =
  "api" :> "createOrderRequest"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] CreateOrderReq
    :> Post '[JSON] CreateOrderRes

createOrderAPI :: Proxy CreateOrderAPI
createOrderAPI = Proxy

createOrder :: HasCoreMetrics r => Token -> BaseUrl -> CreateOrderReq -> FlowR r (Either ClientError CreateOrderRes)
createOrder token url req = callAPI url createOrderReq "createOrder"
  where
    createOrderReq = T.client createOrderAPI (Just token) req
