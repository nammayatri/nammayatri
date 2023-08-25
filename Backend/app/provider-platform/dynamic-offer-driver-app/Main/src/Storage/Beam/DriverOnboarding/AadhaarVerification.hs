{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DriverOnboarding.AadhaarVerification where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Sequelize

data AadhaarVerificationT f = AadhaarVerificationT
  { driverId :: B.C f Text,
    driverName :: B.C f Text,
    driverGender :: B.C f Text,
    aadhaarNumberHash :: B.C f (Maybe DbHash),
    driverDob :: B.C f Text,
    driverImage :: B.C f (Maybe Text),
    isVerified :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarVerificationT where
  data PrimaryKey AadhaarVerificationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type AadhaarVerification = AadhaarVerificationT Identity

aadhaarVerificationTMod :: AadhaarVerificationT (B.FieldModification (B.TableField AadhaarVerificationT))
aadhaarVerificationTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      driverName = B.fieldNamed "driver_name",
      driverGender = B.fieldNamed "driver_gender",
      aadhaarNumberHash = B.fieldNamed "aadhaar_number_hash",
      driverDob = B.fieldNamed "driver_dob",
      driverImage = B.fieldNamed "driver_image",
      isVerified = B.fieldNamed "is_verified",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''AadhaarVerificationT ['driverId] [['aadhaarNumberHash]])

$(mkTableInstances ''AadhaarVerificationT "aadhaar_verification" "atlas_driver_offer_bpp")
