{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Cancel where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Option
import Beckn.Types.Core.Policy
import Beckn.Types.Core.Price
import Beckn.Types.FMD.API.Callback
import Beckn.Types.FMD.Order
import Beckn.Utils.Servant.HeaderAuth
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CancelAPI v =
  "cancel"
    :> APIKeyAuth v
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] CancelRes

cancelAPI :: Proxy (CancelAPI v)
cancelAPI = Proxy

type OnCancelAPI v =
  "on_cancel"
    :> APIKeyAuth v
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

onCancelAPI :: Proxy (OnCancelAPI v)
onCancelAPI = Proxy

data CancelReq = CancelReq
  { context :: Context,
    message :: CancelReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

type OnCancelReq = CallbackReq CancelResMessage

data CancelReqMessage = CancelReqMessage
  { order_id :: Text,
    reason_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnCancelRes = AckResponse

data CancelResMessage = CancelResMessage
  { policies :: [Policy],
    reasons :: [Option],
    price :: Price,
    order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
