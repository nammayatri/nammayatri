{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate where

import qualified Database.Beam as B
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { id :: B.C f Text,
    documentImageId :: B.C f Text,
    certificateNumberEncrypted :: B.C f Text,
    certificateNumberHash :: B.C f DbHash,
    fitnessExpiry :: B.C f UTCTime,
    permitExpiry :: B.C f (Maybe UTCTime),
    pucExpiry :: B.C f (Maybe UTCTime),
    insuranceValidity :: B.C f (Maybe UTCTime),
    vehicleClass :: B.C f (Maybe Text),
    vehicleVariant :: B.C f (Maybe Variant),
    vehicleManufacturer :: B.C f (Maybe Text),
    vehicleCapacity :: B.C f (Maybe Int),
    vehicleModel :: B.C f (Maybe Text),
    vehicleColor :: B.C f (Maybe Text),
    vehicleEnergyType :: B.C f (Maybe Text),
    verificationStatus :: B.C f Domain.VerificationStatus,
    failedRules :: B.C f [Text],
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleRegistrationCertificateT where
  data PrimaryKey VehicleRegistrationCertificateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type VehicleRegistrationCertificate = VehicleRegistrationCertificateT Identity

$(enableKVPG ''VehicleRegistrationCertificateT ['id] [['certificateNumberHash]])

$(mkTableInstances ''VehicleRegistrationCertificateT "vehicle_registration_certificate")
