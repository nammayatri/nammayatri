module Domain.Types.Alert (module Domain.Types.Alert, module Reexport) where

import Domain.Types.Alert.AlertRequestData as Reexport
import Domain.Types.Alert.AlertRequestStatus as Reexport
import Domain.Types.Alert.AlertRequestType as Reexport

castAlertRequestDataToRequestType :: AlertRequestData -> AlertRequestType
castAlertRequestDataToRequestType = \case
  EndRide _ -> EndRideApproval
  OverSpeeding _ -> OverSpeedingAlert
  Stopped _ -> StoppedAlert
  SkippedWaitingStop _ -> SkippedWaitingStopAlert
  MissedStop _ -> MissedStopAlert
  WrongStartStop _ -> WrongStartStopAlert
  RouteDeviation _ -> RouteDeviationAlert
  OppositeDirection _ -> OppositeDirectionAlert
  TripNotStarted _ -> TripNotStartedAlert
  SafetyCheck _ -> SafetyCheckAlert
  RideStopReached _ -> RideStopReachedAlert
