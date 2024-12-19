module Lib.JourneyModule.Utils where

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