{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Vehicle where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Vehicle as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data VehicleT f = VehicleT
  { driverId :: B.C f Text,
    merchantId :: B.C f Text,
    variant :: B.C f Variant.Variant,
    model :: B.C f Text,
    color :: B.C f Text,
    vehicleName :: B.C f (Maybe Text),
    registrationNo :: B.C f Text,
    capacity :: B.C f (Maybe Int),
    category :: B.C f (Maybe Domain.Category),
    make :: B.C f (Maybe Text),
    size :: B.C f (Maybe Text),
    energyType :: B.C f (Maybe Text),
    registrationCategory :: B.C f (Maybe Domain.RegistrationCategory),
    vehicleClass :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleT where
  data PrimaryKey VehicleT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type Vehicle = VehicleT Identity

vehicleTMod :: VehicleT (B.FieldModification (B.TableField VehicleT))
vehicleTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      merchantId = B.fieldNamed "merchant_id",
      variant = B.fieldNamed "variant",
      model = B.fieldNamed "model",
      color = B.fieldNamed "color",
      vehicleName = B.fieldNamed "vehicle_name",
      registrationNo = B.fieldNamed "registration_no",
      capacity = B.fieldNamed "capacity",
      category = B.fieldNamed "category",
      make = B.fieldNamed "make",
      size = B.fieldNamed "size",
      energyType = B.fieldNamed "energy_type",
      registrationCategory = B.fieldNamed "registration_category",
      vehicleClass = B.fieldNamed "vehicle_class",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''VehicleT ['driverId] [['registrationNo]])

$(mkTableInstances ''VehicleT "vehicle" "atlas_driver_offer_bpp")
