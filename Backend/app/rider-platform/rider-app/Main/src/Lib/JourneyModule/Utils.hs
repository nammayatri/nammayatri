module Lib.JourneyModule.Utils where

import qualified Domain.Types.Trip as DTrip
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import Kernel.Utils.Common

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f = go 0
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.MultimodalTravelMode
convertMultiModalModeToTripMode input distance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Subway -> DTrip.Subway
  MultiModal.Walk -> if distance > maximumWalkDistance then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi
