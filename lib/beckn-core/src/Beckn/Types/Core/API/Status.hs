{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Status
  ( module Beckn.Types.Core.API.Status,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Order
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

onStatus :: Proxy OnStatusAPI
onStatus = Proxy

type OnStatusReq = CallbackReq OnStatusReqMessage

type OnStatusRes = AckResponse

data StatusReqMessage = StatusReqMessage
  { service :: IdObject,
    order :: IdObject
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype OnStatusReqMessage = OnStatusReqMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
