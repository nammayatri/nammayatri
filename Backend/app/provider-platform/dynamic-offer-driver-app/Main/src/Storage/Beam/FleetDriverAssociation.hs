{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FleetDriverAssociation where

import qualified Database.Beam as B
--import qualified Domain.Types.FleetDriverAssociation as FleetDriverAssociation
import Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetDriverAssociationT f = FleetDriverAssociationT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    fleetOwnerId :: B.C f Text,
    isActive :: B.C f Bool,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetDriverAssociationT where
  data PrimaryKey FleetDriverAssociationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FleetDriverAssociation = FleetDriverAssociationT Identity

$(enableKVPG ''FleetDriverAssociationT ['id] [])

$(mkTableInstances ''FleetDriverAssociationT "fleet_driver_vehicle_association")
