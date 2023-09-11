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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.RiderDetails where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data RiderDetailsT f = RiderDetailsT
  { id :: B.C f Text,
    mobileCountryCode :: B.C f Text,
    mobileNumberEncrypted :: B.C f Text,
    mobileNumberHash :: B.C f DbHash,
    merchantId :: B.C f Text,
    referralCode :: B.C f (Maybe Text),
    referredByDriver :: B.C f (Maybe Text),
    referredAt :: B.C f (Maybe Time.UTCTime),
    hasTakenValidRide :: B.C f Bool,
    hasTakenValidRideAt :: B.C f (Maybe Time.UTCTime),
    otpCode :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderDetailsT where
  data PrimaryKey RiderDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RiderDetails = RiderDetailsT Identity

riderDetailsTMod :: RiderDetailsT (B.FieldModification (B.TableField RiderDetailsT))
riderDetailsTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      mobileCountryCode = B.fieldNamed "mobile_country_code",
      mobileNumberEncrypted = B.fieldNamed "mobile_number_encrypted",
      mobileNumberHash = B.fieldNamed "mobile_number_hash",
      merchantId = B.fieldNamed "merchant_id",
      referralCode = B.fieldNamed "referral_code",
      referredByDriver = B.fieldNamed "referred_by_driver",
      referredAt = B.fieldNamed "referred_at",
      hasTakenValidRide = B.fieldNamed "has_taken_valid_ride",
      hasTakenValidRideAt = B.fieldNamed "has_taken_valid_ride_at",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      otpCode = B.fieldNamed "otp_code"
    }

$(enableKVPG ''RiderDetailsT ['id] [['mobileNumberHash, 'merchantId]])

$(mkTableInstances ''RiderDetailsT "rider_details" "atlas_driver_offer_bpp")
