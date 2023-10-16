{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.DriverOnboarding.VehicleRegistrationCertificate where

import Domain.Types.DriverOnboarding.IdfyVerification
import qualified Domain.Types.DriverOnboarding.Image as Image
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id

data VehicleRegistrationCertificateE e = VehicleRegistrationCertificate
  { id :: Id VehicleRegistrationCertificate,
    documentImageId :: Id Image.Image,
    certificateNumber :: EncryptedHashedField e Text,
    fitnessExpiry :: UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    insuranceValidity :: Maybe UTCTime,
    vehicleClass :: Maybe Text,
    vehicleVariant :: Maybe Variant,
    failedRules :: [Text],
    vehicleManufacturer :: Maybe Text,
    vehicleCapacity :: Maybe Int,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleEnergyType :: Maybe Text,
    verificationStatus :: VerificationStatus,
    fleetOwnerId :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type VehicleRegistrationCertificate = VehicleRegistrationCertificateE 'AsEncrypted

type DecryptedVehicleRegistrationCertificate = VehicleRegistrationCertificateE 'AsUnencrypted

deriving instance Show DecryptedVehicleRegistrationCertificate

data VehicleRegistrationCertificateAPIEntity = VehicleRegistrationCertificateAPIEntity
  { certificateNumber :: Text,
    fitnessExpiry :: UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    insuranceValidity :: Maybe UTCTime,
    vehicleClass :: Maybe Text,
    vehicleVariant :: Maybe Variant,
    failedRules :: [Text],
    vehicleManufacturer :: Maybe Text,
    vehicleCapacity :: Maybe Int,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleEnergyType :: Maybe Text,
    verificationStatus :: VerificationStatus,
    fleetOwnerId :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance EncryptedItem VehicleRegistrationCertificate where
  type Unencrypted VehicleRegistrationCertificate = (DecryptedVehicleRegistrationCertificate, HashSalt)
  encryptItem (VehicleRegistrationCertificate {..}, salt) = do
    certificateNumber_ <- encryptItem $ (,salt) certificateNumber
    return VehicleRegistrationCertificate {certificateNumber = certificateNumber_, ..}
  decryptItem VehicleRegistrationCertificate {..} = do
    certificateNumber_ <- fst <$> decryptItem certificateNumber
    return (VehicleRegistrationCertificate {certificateNumber = certificateNumber_, ..}, "")

instance EncryptedItem' VehicleRegistrationCertificate where
  type UnencryptedItem VehicleRegistrationCertificate = DecryptedVehicleRegistrationCertificate
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

makeRCAPIEntity :: VehicleRegistrationCertificate -> Text -> VehicleRegistrationCertificateAPIEntity
makeRCAPIEntity VehicleRegistrationCertificate {..} rcDecrypted =
  VehicleRegistrationCertificateAPIEntity
    { certificateNumber = rcDecrypted,
      ..
    }

makeVehicleFromRC :: UTCTime -> Id Person -> Id Merchant -> Text -> VehicleRegistrationCertificate -> Vehicle
makeVehicleFromRC now driverId merchantId certificateNumber rc =
  Vehicle
    { driverId,
      capacity = rc.vehicleCapacity,
      category = getCategory <$> rc.vehicleVariant,
      make = rc.vehicleManufacturer,
      model = fromMaybe "Unkown" rc.vehicleModel,
      size = Nothing,
      merchantId,
      variant = fromMaybe AUTO_RICKSHAW rc.vehicleVariant, -- Value will be always Just if reaching here
      color = fromMaybe "Unkown" rc.vehicleColor,
      energyType = rc.vehicleEnergyType,
      registrationNo = certificateNumber,
      registrationCategory = Nothing,
      vehicleClass = fromMaybe "Unkown" rc.vehicleClass,
      vehicleName = Nothing,
      createdAt = now,
      updatedAt = now
    }
