module Beckn.Types.Core.Migration.API.Cancel where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq, OrderObject)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] (BecknReq CancellationInfo)
    :> Post '[JSON] AckResponse

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

data CancellationInfo = CancellationInfo
  { order_id :: Text,
    cancellation_reason_id :: Text,
    descriptor :: Maybe Descriptor
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OrderObject)
    :> Post '[JSON] AckResponse

onCancelAPI :: Proxy OnCancelAPI
onCancelAPI = Proxy
