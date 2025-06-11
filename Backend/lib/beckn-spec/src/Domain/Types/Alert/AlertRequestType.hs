module Domain.Types.Alert.AlertRequestType where

import Kernel.Prelude

data AlertRequestType = EndRideApproval | OverSpeedingAlert | StoppedAlert | SkippedWaitingStopAlert | MissedStopAlert | WrongStartStopAlert | RouteDeviationAlert | OppositeDirectionAlert | TripNotStartedAlert
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
