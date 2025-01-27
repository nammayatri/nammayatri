module Lib.JourneyModule.Utils where

import qualified Domain.Types.Trip as DTrip
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import Kernel.Utils.Common

-- replaceCurrentWithAuto :: Journey -> VehicleType -> m () -- add vehicleType
-- replaceCurrentWithAuto journey vehicleType = do
--   currentLeg <- getCurrentLeg journey
--   let currentState = getState currentLeg
--   let autoLeg = mkAutoLeg currentState.currentPosition currentLeg vehicleType
--   replaceLeg journey [currentLeg] autoLeg

-- replaceAllWithAuto :: Journey -> VehicleType -> m () -- add vehicleType
-- replaceAllWithAuto journey vehicleType = do
--   currentLeg <- getCurrentLeg journey
--   let currentState = getState currentLeg
--   legs <- getRemaningLegs journey
--   let lastLegInfo = get legs[length legs - 1]
--   let autoLeg = mkAutoLeg currentState.currentPosition lastLegInfo vehicleType
--   replaceLeg journey legs autoLeg

-- continueWithAuto :: Journey -> m ()
-- continueWithAuto journey = do
--   currentAutoLeg <- getCurrentLeg journey
--   legs <- getRemaningLegs journey
--   let lastLegInfo = get legs[length legs - 1]
--   let autoLeg = currentAutoLeg { dropPoint = lastLegInfo.endLocation}
--   extendLeg journey legs autoLeg

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
  MultiModal.Subway -> DTrip.Metro
  MultiModal.Walk -> if distance > maximumWalkDistance then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi
