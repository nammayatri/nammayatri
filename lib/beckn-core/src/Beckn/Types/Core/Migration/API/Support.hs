module Beckn.Types.Core.Migration.API.Support where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SupportAPI =
  "support"
    :> ReqBody '[JSON] (BecknReq IdRequiringSupport)
    :> Post '[JSON] AckResponse

supportAPI :: Proxy SupportAPI
supportAPI = Proxy

newtype IdRequiringSupport = IdRequiringSupport
  { ref_id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnSupportAPI =
  "on_support"
    :> ReqBody '[JSON] (BecknCallbackReq SupportInfo)
    :> Post '[JSON] AckResponse

onSupportAPI :: Proxy OnSupportAPI
onSupportAPI = Proxy

data SupportInfo = SupportInfo
  { phone :: Maybe Text,
    email :: Maybe Text,
    uri :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
