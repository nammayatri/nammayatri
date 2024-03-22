{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverOnboarding.DriverRCAssociation where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverRCAssociationT f = DriverRCAssociationT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    rcId :: B.C f Text,
    associatedOn :: B.C f UTCTime,
    associatedTill :: B.C f (Maybe UTCTime),
    consent :: B.C f Bool,
    consentTimestamp :: B.C f UTCTime,
    isRcActive :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverRCAssociationT where
  data PrimaryKey DriverRCAssociationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverRCAssociation = DriverRCAssociationT Identity

$(enableKVPG ''DriverRCAssociationT ['id] [['driverId], ['rcId]])

$(mkTableInstances ''DriverRCAssociationT "driver_rc_association")
