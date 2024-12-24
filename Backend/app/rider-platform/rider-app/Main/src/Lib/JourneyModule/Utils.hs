module Lib.JourneyModule.Utils where

import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import qualified Domain.Types.Trip as DTrip
import Kernel.Utils.Common

-- Journey Module (utils function)
-- replace auto with taxi / generic name
replaceCurrentWithAuto :: Journey -> VehicleType -> m () -- add vehicleType
replaceCurrentWithAuto journey vehicleType = do
  currentLeg <- getCurrentLeg journey
  let currentState = getState currentLeg
  let autoLeg = mkAutoLeg currentState.currentPosition currentLeg vehicleType
  replaceLeg journey [currentLeg] autoLeg

replaceAllWithAuto :: Journey -> VehicleType -> m () -- add vehicleType
replaceAllWithAuto journey vehicleType = do
  currentLeg <- getCurrentLeg journey
  let currentState = getState currentLeg
  legs <- getRemaningLegs journey
  let lastLegInfo = get legs[length legs - 1]
  let autoLeg = mkAutoLeg currentState.currentPosition lastLegInfo vehicleType
  replaceLeg journey legs autoLeg

continueWithAuto :: Journey -> m ()
continueWithAuto journey = do
  currentAutoLeg <- getCurrentLeg journey
  legs <- getRemaningLegs journey
  let lastLegInfo = get legs[length legs - 1]
  let autoLeg = currentAutoLeg { dropPoint = lastLegInfo.endLocation}
  extendLeg journey legs autoLeg



-- what to be shown in case of skip on UI

updateJourney :: Journey -> Id Journey -> m ()-- to update journey data


recalculate :: Journey -> m ()
let legs = getRemainingLegs
-- for now nothing just call replace with auto

-- completeCurrentLeg :: Journey -> m ()

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f xs = go 0 xs
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.TravelMode
convertMultiModalModeToTripMode input distance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Walk -> if (distance > maximumWalkDistance) then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi