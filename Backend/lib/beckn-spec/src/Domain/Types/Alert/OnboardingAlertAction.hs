module Domain.Types.Alert.OnboardingAlertAction where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data OnboardingAlertAction
  = LinkVehicleAction
  | UnlinkVehicleAction
  | ActivateVehicleAction
  | DeactivateVehicleAction
  | LinkToOperatorAction
  | UnlinkFromOperatorAction
  | UnlinkFromFleetAction
  | AddAction
  | DeleteAction
  | EnableAction
  | DisableAction
  | BlockAction
  | UnblockAction
  | ApproveAction
  | RejectAction
  | SetOnboardingAsAction
  | LinkToFleetAction
  | ActivateToFleetAction
  | DeactivateFromFleetAction
  | ViewAction
  | ChangeFleetOwnerAction
  | ExpireAction
  | UnlinkDocumentAction
  | OnboardingFlagMutationAction
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''OnboardingAlertAction)

$(mkHttpInstancesForEnum ''OnboardingAlertAction)
