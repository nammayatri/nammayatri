{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Cancel where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import Beckn.Types.FMD.Order
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

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
