module API.External.LiveEKD where

import Domain.Types.External.LiveEKD
import EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess
import Servant (Header, JSON, Post, ReqBody, (:>))

type LiveEKDAPI =
  "internal"
    :> "vocalytics"
    :> "upload"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] LiveEKDRequest
    :> Post '[JSON] APISuccess

liveEKDAPI :: Proxy LiveEKDAPI
liveEKDAPI = Proxy

liveEKD :: Maybe Text -> LiveEKDRequest -> Euler.EulerClient APISuccess
liveEKD = Euler.client liveEKDAPI
