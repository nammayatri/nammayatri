module Beckn.Types.Core.Taxi.Track
  ( module Beckn.Types.Core.Taxi.Track,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Track.Order as Reexport

newtype TrackMessage = TrackMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
