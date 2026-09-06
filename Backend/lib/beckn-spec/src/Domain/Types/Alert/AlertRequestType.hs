module Domain.Types.Alert.AlertRequestType where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data AlertRequestType = EndRideApproval | OverSpeedingAlert | StoppedAlert | SkippedWaitingStopAlert | MissedStopAlert | WrongStartStopAlert | RouteDeviationAlert | OppositeDirectionAlert | TripNotStartedAlert | SafetyCheckAlert | RideStopReachedAlert | GenericAlert | OnboardingAlert
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''AlertRequestType)

$(mkHttpInstancesForEnum ''AlertRequestType)
