module Beckn.Types.Core.Taxi.Confirm.Req
  ( module Beckn.Types.Core.Taxi.Confirm.Req,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Confirm.Req.Order as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype ConfirmReqMessage = ConfirmReqMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
