module Beckn.Types.Core.Taxi.OnTrack
  ( module Beckn.Types.Core.Taxi.OnTrack,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnTrack.Tracking as Reexport
import Kernel.Prelude

newtype OnTrackMessage = OnTrackMessage
  { tracking :: Tracking
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
