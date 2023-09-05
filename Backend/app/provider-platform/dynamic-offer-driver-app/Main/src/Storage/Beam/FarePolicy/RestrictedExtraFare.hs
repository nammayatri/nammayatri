{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FarePolicy.RestrictedExtraFare where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Sequelize
import Tools.Beam.UtilsTH

data RestrictedExtraFareT f = RestrictedExtraFareT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    vehicleVariant :: B.C f Vehicle.Variant,
    minTripDistance :: B.C f Meters,
    driverMaxExtraFare :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table RestrictedExtraFareT where
  data PrimaryKey RestrictedExtraFareT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RestrictedExtraFare = RestrictedExtraFareT Identity

$(enableKVPG ''RestrictedExtraFareT ['id] [])

$(mkTableInstances ''RestrictedExtraFareT "restricted_extra_fare")
