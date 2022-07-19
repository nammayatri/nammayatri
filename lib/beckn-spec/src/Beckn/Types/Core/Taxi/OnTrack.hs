module Beckn.Types.Core.Taxi.OnTrack
  ( module Beckn.Types.Core.Taxi.OnTrack,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnTrack.Tracking as Reexport

newtype OnTrackMessage = OnTrackMessage
  { tracking :: Tracking
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
