{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.RiderDetails where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import Tools.Beam.UtilsTH

data RiderDetailsT f = RiderDetailsT
  { id :: B.C f Text,
    mobileCountryCode :: B.C f Text,
    mobileNumberEncrypted :: B.C f Text,
    mobileNumberHash :: B.C f DbHash,
    merchantId :: B.C f Text,
    referralCode :: B.C f (Maybe Text),
    referredByDriver :: B.C f (Maybe Text),
    referredAt :: B.C f (Maybe UTCTime),
    hasTakenValidRide :: B.C f Bool,
    hasTakenValidRideAt :: B.C f (Maybe UTCTime),
    otpCode :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderDetailsT where
  data PrimaryKey RiderDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RiderDetails = RiderDetailsT Identity

$(enableKVPG ''RiderDetailsT ['id] [['mobileNumberHash, 'merchantId]])

$(mkTableInstances ''RiderDetailsT "rider_details")
