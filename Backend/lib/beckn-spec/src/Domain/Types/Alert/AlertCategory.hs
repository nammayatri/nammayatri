module Domain.Types.Alert.AlertCategory where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data AlertCategory
  = WMB_ALERT
  | ONBOARDING_UPDATE
  | FLEET_UPDATE
  | GENERIC_NOTIFICATION
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''AlertCategory)

$(mkHttpInstancesForEnum ''AlertCategory)
