module Beckn.Types.Core.Migration1.Confirm
  ( module Beckn.Types.Core.Migration1.Confirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.Confirm.Order as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
