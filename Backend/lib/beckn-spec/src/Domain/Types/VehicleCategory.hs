module Domain.Types.VehicleCategory where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data VehicleCategory
  = CAR
  | MOTORCYCLE
  | TRAIN
  | BUS
  | FLIGHT
  | AUTO_CATEGORY
  | AMBULANCE
  | TRUCK
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''VehicleCategory)
