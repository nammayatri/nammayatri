{-# LANGUAGE TypeApplications #-}

module ExternalAPI.Dunzo.Flow where

import Beckn.Types.Common
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified EulerHS.Types as T
import ExternalAPI.Dunzo.Types
import Servant (Capture, Get, Header, JSON, NoContent, Post, QueryParam, ReqBody, (:>))
import Types.Common
import Types.Metrics (CoreMetrics)

type GetTokenAPI =
  "api" :> "v1" :> "token"
    :> Header "client-id" Text
    :> Header "client-secret" Text
    :> Get '[JSON] TokenRes

getTokenAPI :: Proxy GetTokenAPI
getTokenAPI = Proxy

getToken ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  TokenReq ->
  m TokenRes
getToken url req = callDunzoAPI url tokenReq "getToken"
  where
    clientId = Just $ getClientId $ req.client_id
    clientSecret = Just $ getClientSecret $ req.client_secret
    tokenReq = T.client getTokenAPI clientId clientSecret

type QuoteAPI =
  "api" :> "v1" :> "quote"
    :> Header "Authorization" Token
    :> Header "client-id" ClientId
    :> QueryParam "pickup_lat" Double
    :> QueryParam "pickup_lng" Double
    :> QueryParam "drop_lat" Double
    :> QueryParam "drop_lng" Double
    :> QueryParam "category_id" Text
    :> Get '[JSON] QuoteRes

quoteAPI :: Proxy QuoteAPI
quoteAPI = Proxy

getQuote ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ClientId ->
  Token ->
  BaseUrl ->
  QuoteReq ->
  m QuoteRes
getQuote clientId token url req = callDunzoAPI url quoteReq "getQuote"
  where
    quoteReq =
      T.client
        quoteAPI
        (Just token)
        (Just clientId)
        (Just $ req.pickup_lat)
        (Just $ req.pickup_lng)
        (Just $ req.drop_lat)
        (Just $ req.drop_lng)
        (Just $ req.category_id)

type CreateTaskAPI =
  "api" :> "v1" :> "tasks"
    :> Header "Authorization" Token
    :> Header "client-id" ClientId
    :> QueryParam "test" Bool
    :> ReqBody '[JSON] CreateTaskReq
    :> Post '[JSON] CreateTaskRes

createTaskAPI :: Proxy CreateTaskAPI
createTaskAPI = Proxy

createTask ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ClientId ->
  Token ->
  BaseUrl ->
  Bool ->
  CreateTaskReq ->
  m CreateTaskRes
createTask clientId token url isTestMode req = callDunzoAPI url task "createTask"
  where
    task =
      T.client
        createTaskAPI
        (Just token)
        (Just clientId)
        (setTestQueryParam isTestMode)
        req

type TaskStatusAPI =
  "api" :> "v1" :> "tasks"
    :> Capture "task_id" TaskId
    :> "status"
    :> Header "Authorization" Token
    :> Header "client-id" ClientId
    :> QueryParam "test" Bool
    :> Get '[JSON] TaskStatus

taskStatusAPI :: Proxy TaskStatusAPI
taskStatusAPI = Proxy

taskStatus ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ClientId ->
  Token ->
  BaseUrl ->
  Bool ->
  TaskId ->
  m TaskStatus
taskStatus clientId token url isTestMode taskId = callDunzoAPI url status "taskStatus"
  where
    status =
      T.client
        taskStatusAPI
        taskId
        (Just token)
        (Just clientId)
        (setTestQueryParam isTestMode)

type CancelTaskAPI =
  "api" :> "v1" :> "tasks"
    :> Capture "task_id" TaskId
    :> "_cancel"
    :> Header "Authorization" Token
    :> Header "client-id" ClientId
    :> QueryParam "test" Bool
    :> ReqBody '[JSON] CancelTaskReq
    :> Post '[JSON] NoContent

cancelTaskAPI :: Proxy CancelTaskAPI
cancelTaskAPI = Proxy

cancelTask ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ClientId ->
  Token ->
  BaseUrl ->
  Bool ->
  TaskId ->
  Text ->
  m ()
cancelTask clientId token url isTestMode taskId cancellationReason = callDunzoAPI url cancel "cancelTask"
  where
    cancel =
      void $
        T.client
          cancelTaskAPI
          taskId
          (Just token)
          (Just clientId)
          (setTestQueryParam isTestMode)
          (CancelTaskReq cancellationReason)

setTestQueryParam :: Bool -> Maybe Bool
setTestQueryParam isTestMode = if isTestMode then Just True else Nothing

callDunzoAPI :: CallAPI env res
callDunzoAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing
