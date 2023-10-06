{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverOnboarding.AadhaarVerification where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data AadhaarVerificationT f = AadhaarVerificationT
  { driverId :: B.C f Text,
    driverName :: B.C f Text,
    driverGender :: B.C f Text,
    aadhaarNumberHash :: B.C f (Maybe DbHash),
    driverDob :: B.C f Text,
    driverImage :: B.C f (Maybe Text),
    isVerified :: B.C f Bool,
    driverImagePath :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarVerificationT where
  data PrimaryKey AadhaarVerificationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type AadhaarVerification = AadhaarVerificationT Identity

$(TH.enableKVPG ''AadhaarVerificationT ['driverId] [['aadhaarNumberHash]])

$(TH.mkTableInstances ''AadhaarVerificationT "aadhaar_verification")
