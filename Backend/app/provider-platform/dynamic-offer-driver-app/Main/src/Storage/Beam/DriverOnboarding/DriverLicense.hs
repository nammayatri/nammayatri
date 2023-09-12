{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverOnboarding.DriverLicense where

import qualified Database.Beam as B
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import Kernel.External.Encryption
import Kernel.Prelude
import Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate ()
import Tools.Beam.UtilsTH

data DriverLicenseT f = DriverLicenseT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    documentImageId1 :: B.C f Text,
    documentImageId2 :: B.C f (Maybe Text),
    driverDob :: B.C f (Maybe UTCTime),
    driverName :: B.C f (Maybe Text),
    licenseNumberEncrypted :: B.C f Text,
    licenseNumberHash :: B.C f DbHash,
    licenseExpiry :: B.C f UTCTime,
    classOfVehicles :: B.C f [Text],
    failedRules :: B.C f [Text],
    verificationStatus :: B.C f Domain.VerificationStatus,
    consent :: B.C f Bool,
    consentTimestamp :: B.C f UTCTime,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverLicenseT where
  data PrimaryKey DriverLicenseT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverLicense = DriverLicenseT Identity

$(enableKVPG ''DriverLicenseT ['id] [['driverId], ['licenseNumberHash]])

$(mkTableInstances ''DriverLicenseT "driver_license")
