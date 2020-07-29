{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Status where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Duration
import Beckn.Types.Core.Error
import Beckn.Types.FMD.Order
import Beckn.Utils.Servant.Auth
import Data.Generics.Labels ()
import Data.Time.LocalTime
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

data OnStatusReq = OnStatusReq
  { context :: Context,
    message :: StatusResMessage,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype StatusReqMessage = StatusReqMessage
  { order_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnStatusRes = AckResponse

data StatusResMessage = StatusResMessage
  { order :: Order,
    updated_at :: LocalTime,
    status_refresh :: Duration
  }
  deriving (Generic, Show, ToJSON, FromJSON)
