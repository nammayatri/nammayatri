module Core.OnCancel (module Core.OnCancel, module OnStatus) where

import Beckn.Prelude
import Core.OnStatus as OnStatus

newtype OnCancelMessage = OnCancelMessage
  { order :: OnStatus.Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
