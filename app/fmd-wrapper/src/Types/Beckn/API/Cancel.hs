{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Cancel where

import Beckn.Types.Core.Ack
import Data.Generics.Labels ()
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Context
import Types.Beckn.FmdOrder

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] CancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

onCancelAPI :: Proxy OnCancelAPI
onCancelAPI = Proxy

data CancelReq = CancelReq
  { context :: Context,
    message :: CancelReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

type OnCancelReq = CallbackReq CancelResMessage

newtype CancelReqMessage = CancelReqMessage
  { order :: CancelOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancelOrder = CancelOrder
  { id :: Text,
    cancellation_reason_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnCancelRes = AckResponse

newtype CancelResMessage = CancelResMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
