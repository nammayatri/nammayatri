module Domain.Types.Alert (module Domain.Types.Alert, module Reexport) where

import Domain.Types.Alert.AlertCategory as Reexport
import Domain.Types.Alert.AlertEntityType as Reexport
import Domain.Types.Alert.AlertRequestData as Reexport
import Domain.Types.Alert.AlertRequestStatus as Reexport
import Domain.Types.Alert.AlertRequestType as Reexport
import Domain.Types.Alert.OnboardingAlertAction as Reexport

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
  GenericNotification _ -> GenericAlert
  Onboarding _ -> OnboardingAlert

castAlertRequestTypeToCategory :: AlertRequestType -> AlertCategory
castAlertRequestTypeToCategory = \case
  OnboardingAlert -> ONBOARDING_UPDATE
  GenericAlert -> GENERIC_NOTIFICATION
  _ -> WMB_ALERT
