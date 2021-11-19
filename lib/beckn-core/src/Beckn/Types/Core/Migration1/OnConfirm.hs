module Beckn.Types.Core.Migration1.OnConfirm
  ( module Beckn.Types.Core.Migration1.OnConfirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.OnConfirm.Order as Reexport
import Beckn.Types.Core.Migration1.OnConfirm.Provider as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
