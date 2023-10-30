{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.DriverOnboarding.DriverLicense where

import Domain.Types.DriverOnboarding.IdfyVerification
import qualified Domain.Types.DriverOnboarding.Image as Image
import Domain.Types.Person (Person)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id

data DriverLicenseE e = DriverLicense
  { id :: Id DriverLicense,
    driverId :: Id Person,
    documentImageId1 :: Id Image.Image,
    documentImageId2 :: Maybe (Id Image.Image),
    driverDob :: Maybe UTCTime,
    driverName :: Maybe Text,
    licenseNumber :: EncryptedHashedField e Text,
    licenseExpiry :: UTCTime,
    classOfVehicles :: [Text],
    failedRules :: [Text],
    verificationStatus :: VerificationStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    consent :: Bool,
    consentTimestamp :: UTCTime
  }
  deriving (Generic)

type DriverLicense = DriverLicenseE 'AsEncrypted

type DecryptedDriverLicense = DriverLicenseE 'AsUnencrypted

instance EncryptedItem DriverLicense where
  type Unencrypted DriverLicense = (DecryptedDriverLicense, HashSalt)
  encryptItem (DriverLicense {..}, salt) = do
    licenseNumber_ <- encryptItem (licenseNumber, salt)
    return DriverLicense {licenseNumber = licenseNumber_, ..}
  decryptItem DriverLicense {..} = do
    licenseNumber_ <- fst <$> decryptItem licenseNumber
    return (DriverLicense {licenseNumber = licenseNumber_, ..}, "")

instance EncryptedItem' DriverLicense where
  type UnencryptedItem DriverLicense = DecryptedDriverLicense
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
