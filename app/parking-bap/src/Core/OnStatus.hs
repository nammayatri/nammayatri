module Core.OnStatus
  ( module Core.OnStatus,
  )
where

import Beckn.Prelude
import Core.Order

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
