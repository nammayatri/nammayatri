module Beckn.Types.Core.Cabs.OnConfirm
  ( module Beckn.Types.Core.Cabs.OnConfirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.OnConfirm.Order as Reexport
import Beckn.Types.Core.Cabs.OnConfirm.Provider as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
