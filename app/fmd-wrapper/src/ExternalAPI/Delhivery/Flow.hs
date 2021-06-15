{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module ExternalAPI.Delhivery.Flow where

import Beckn.Types.Common
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified EulerHS.Types as T
import ExternalAPI.Delhivery.Types
import Servant (FormUrlEncoded, Header, JSON, Post, ReqBody, (:>))
import Types.Common
import Types.Metrics

type TokenAPI =
  "auth" :> "realms" :> "HLD-Test" :> "protocol" :> "openid-connect" :> "token"
    :> ReqBody '[FormUrlEncoded] TokenReq
    :> Post '[JSON] TokenRes

tokenAPI :: Proxy TokenAPI
tokenAPI = Proxy

getToken ::
  HasCoreMetrics r =>
  BaseUrl ->
  TokenReq ->
  FlowR r TokenRes
getToken url req = callDelhiveryAPI url tokenReq "getToken"
  where
    tokenReq = T.client tokenAPI req

type QuoteAPI =
  "api" :> "mileage"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] QuoteReq
    :> Post '[JSON] QuoteRes

quoteAPI :: Proxy QuoteAPI
quoteAPI = Proxy

getQuote ::
  HasCoreMetrics r =>
  Token ->
  BaseUrl ->
  QuoteReq ->
  FlowR r QuoteRes
getQuote token url req = callDelhiveryAPI url quoteReq "getQuote"
  where
    quoteReq = T.client quoteAPI (Just token) req

type CreateOrderAPI =
  "api" :> "createOrderRequest"
    :> Header "Authorization" Token
    :> ReqBody '[JSON] CreateOrderReq
    :> Post '[JSON] CreateOrderRes

createOrderAPI :: Proxy CreateOrderAPI
createOrderAPI = Proxy

createOrder ::
  HasCoreMetrics r =>
  Token ->
  BaseUrl ->
  CreateOrderReq ->
  FlowR r CreateOrderRes
createOrder token url req = callDelhiveryAPI url createOrderReq "createOrder"
  where
    createOrderReq = T.client createOrderAPI (Just token) req

callDelhiveryAPI :: CallAPI env res
callDelhiveryAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing
