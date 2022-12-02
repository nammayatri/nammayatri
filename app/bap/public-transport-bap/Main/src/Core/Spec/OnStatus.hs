module Core.Spec.OnStatus
  ( module Core.Spec.OnStatus,
    module Reexport,
  )
where

import Beckn.Prelude
import Core.Spec.OnStatus.Descriptor as Reexport
import Core.Spec.OnStatus.Item as Reexport
import Core.Spec.OnStatus.Order as Reexport
import Core.Spec.OnStatus.Params as Reexport
import Core.Spec.OnStatus.Time as Reexport

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
