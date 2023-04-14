module Beckn.Types.Core.Taxi.API.CancellationReasons where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CancellationReasonsAPI =
  "get_cancellation_reasons"
    :> ReqBody '[JSON] CancellationReasonsReq
    :> Post '[JSON] CancellationReasons

cancellationReasonsAPI :: Proxy CancellationReasonsAPI
cancellationReasonsAPI = Proxy
