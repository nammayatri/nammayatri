module Domain.Types.Alert.AlertEntityType where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data AlertEntityType
  = DriverEntity
  | VehicleEntity
  | FleetEntity
  | TripTransactionEntity
  | RideEntity
  | ConfigChangeEntity
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''AlertEntityType)

$(mkHttpInstancesForEnum ''AlertEntityType)
