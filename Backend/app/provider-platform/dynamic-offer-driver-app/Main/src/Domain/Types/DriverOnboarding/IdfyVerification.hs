{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.DriverOnboarding.IdfyVerification where

import Database.Beam (FromBackendRow)
import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax, autoSqlValueSyntax, sqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person
import Domain.Types.Vehicle as Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data VerificationStatus = PENDING | VALID | INVALID
  deriving stock (Show, Eq, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ImageExtractionValidation = Success | Skipped | Failed
  deriving stock (Show, Eq, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance FromField VerificationStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be VerificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromField ImageExtractionValidation where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ImageExtractionValidation where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be ImageExtractionValidation

instance FromBackendRow Postgres ImageExtractionValidation

instance BeamSqlBackend be => B.HasSqlEqualityCheck be VerificationStatus

instance FromBackendRow Postgres VerificationStatus

instance IsString ImageExtractionValidation where
  fromString = show

data IdfyVerificationE e = IdfyVerification
  { id :: Id IdfyVerification,
    documentImageId1 :: Id Image,
    documentImageId2 :: Maybe (Id Image),
    driverId :: Id Person,
    requestId :: Text,
    docType :: ImageType,
    status :: Text,
    issueDateOnDoc :: Maybe UTCTime,
    driverDateOfBirth :: Maybe UTCTime,
    documentNumber :: EncryptedHashedField e Text,
    imageExtractionValidation :: ImageExtractionValidation,
    idfyResponse :: Maybe Text,
    multipleRC :: Maybe Bool,
    dashboardPassedVehicleVariant :: Maybe Vehicle.Variant,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type IdfyVerification = IdfyVerificationE 'AsEncrypted

type DecryptedIdfyVerification = IdfyVerificationE 'AsUnencrypted

instance EncryptedItem IdfyVerification where
  type Unencrypted IdfyVerification = (DecryptedIdfyVerification, HashSalt)
  encryptItem (IdfyVerification {..}, salt) = do
    documentNumber_ <- encryptItem $ (,salt) documentNumber
    return IdfyVerification {documentNumber = documentNumber_, ..}
  decryptItem IdfyVerification {..} = do
    documentNumber_ <- fst <$> decryptItem documentNumber
    return (IdfyVerification {documentNumber = documentNumber_, ..}, "")

instance EncryptedItem' IdfyVerification where
  type UnencryptedItem IdfyVerification = DecryptedIdfyVerification
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
