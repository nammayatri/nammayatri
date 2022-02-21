module Core.Spec.OnCancel
  ( module Core.Spec.OnCancel,
    module Reexport,
  )
where

import Beckn.Prelude
import Core.Spec.OnStatus.Descriptor as Reexport
import Core.Spec.OnStatus.Item as Reexport
import Core.Spec.OnStatus.Order as Reexport
import Core.Spec.OnStatus.Params as Reexport
import Core.Spec.OnStatus.Time as Reexport

newtype OnCancelMessage = OnCancelMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
