module Beckn.Types.Core.Taxi.Track
  ( module Beckn.Types.Core.Taxi.Track,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Track.Order as Reexport
import Kernel.Prelude

newtype TrackMessage = TrackMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
