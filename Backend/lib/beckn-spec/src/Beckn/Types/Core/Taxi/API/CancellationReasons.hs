module Beckn.Types.Core.Taxi.API.CancellationReasons where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import EulerHS.Prelude
import Servant (JSON, Post, (:>))

type CancellationReasonsAPI =
  "get_cancellation_reasons"
    :> Post '[JSON] CancellationReasons

cancellationReasonsAPI :: Proxy CancellationReasonsAPI
cancellationReasonsAPI = Proxy
