{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Status where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.Order
import Beckn.Utils.Servant.HeaderAuth
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI v =
  "status"
    :> APIKeyAuth v
    :> ReqBody '[JSON] StatusReq
    :> Post '[JSON] StatusRes

statusAPI :: Proxy (StatusAPI v)
statusAPI = Proxy

type OnStatusAPI v =
  "on_status"
    :> APIKeyAuth v
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

onStatusAPI :: Proxy (OnStatusAPI v)
onStatusAPI = Proxy

data StatusReq = StatusReq
  { context :: Context,
    message :: StatusReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse

type OnStatusReq = CallbackReq StatusResMessage

newtype StatusReqMessage = StatusReqMessage
  { order_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnStatusRes = AckResponse

newtype StatusResMessage = StatusResMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
