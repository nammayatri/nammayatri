module Beckn.Types.Core.Cabs.Confirm
  ( module Beckn.Types.Core.Cabs.Confirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Confirm.Order as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
