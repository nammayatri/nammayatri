{-# LANGUAGE OverloadedLabels #-}

module External.Dunzo.Flow where

import Beckn.Types.Common (FlowR)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import External.Dunzo.Types
import Servant (Capture, Get, Header, JSON, NoContent, Post, QueryParam, ReqBody, (:>))
import Servant.Client (BaseUrl, ClientError)

type GetTokenAPI =
  "api" :> "v1" :> "token"
    :> Header "client-id" Text
    :> Header "client-secret" Text
    :> Get '[JSON] TokenRes

getTokenAPI :: Proxy GetTokenAPI
getTokenAPI = Proxy

getToken :: BaseUrl -> TokenReq -> FlowR e (Either ClientError TokenRes)
getToken url req = L.callAPI url tokenReq
  where
    clientId = Just $ getClientId $ req ^. #client_id
    clientSecret = Just $ getClientSecret $ req ^. #client_secret
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

getQuote :: ClientId -> Token -> BaseUrl -> QuoteReq -> FlowR e (Either ClientError QuoteRes)
getQuote clientId token url req = L.callAPI url quoteReq
  where
    quoteReq =
      T.client
        quoteAPI
        (Just token)
        (Just clientId)
        (Just $ req ^. #pickup_lat)
        (Just $ req ^. #pickup_lng)
        (Just $ req ^. #drop_lat)
        (Just $ req ^. #drop_lng)
        (Just $ req ^. #category_id)

type CreateTaskAPI =
  "api" :> "v1" :> "tasks"
    :> Header "Authorization" Token
    :> Header "client-id" ClientId
    :> QueryParam "test" Bool
    :> ReqBody '[JSON] CreateTaskReq
    :> Post '[JSON] CreateTaskRes

createTaskAPI :: Proxy CreateTaskAPI
createTaskAPI = Proxy

createTask :: ClientId -> Token -> BaseUrl -> Bool -> CreateTaskReq -> FlowR e (Either ClientError CreateTaskRes)
createTask clientId token url isTestMode req = L.callAPI url task
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

taskStatus :: ClientId -> Token -> BaseUrl -> Bool -> TaskId -> FlowR e (Either ClientError TaskStatus)
taskStatus clientId token url isTestMode taskId = L.callAPI url status
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

cancelTask :: ClientId -> Token -> BaseUrl -> Bool -> TaskId -> Text -> FlowR e (Either ClientError ())
cancelTask clientId token url isTestMode taskId cancellationReason = L.callAPI url cancel
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
