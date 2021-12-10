module Beckn.Types.Core.Taxi.Confirm.Res
  ( module Beckn.Types.Core.Taxi.Confirm.Res,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Confirm.Res.Order as Reexport
import Beckn.Types.Core.Taxi.Confirm.Res.Provider as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype ConfirmResMessage = ConfirmResMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
