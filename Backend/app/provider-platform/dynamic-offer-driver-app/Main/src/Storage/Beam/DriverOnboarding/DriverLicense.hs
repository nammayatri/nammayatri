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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.DriverOnboarding.DriverLicense where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.VerificationStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.VerificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.VerificationStatus

instance FromBackendRow Postgres Domain.VerificationStatus

data DriverLicenseT f = DriverLicenseT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    documentImageId1 :: B.C f Text,
    documentImageId2 :: B.C f (Maybe Text),
    driverDob :: B.C f (Maybe Time.UTCTime),
    driverName :: B.C f (Maybe Text),
    licenseNumberEncrypted :: B.C f Text,
    licenseNumberHash :: B.C f DbHash,
    licenseExpiry :: B.C f Time.UTCTime,
    classOfVehicles :: B.C f [Text],
    failedRules :: B.C f [Text],
    verificationStatus :: B.C f Domain.VerificationStatus,
    consent :: B.C f Bool,
    consentTimestamp :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverLicenseT where
  data PrimaryKey DriverLicenseT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverLicenseT where
  modelFieldModification = driverLicenseTMod
  modelTableName = "driver_license"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverLicense = DriverLicenseT Identity

instance FromJSON DriverLicense where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverLicense where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverLicense

deriving stock instance Ord Domain.VerificationStatus

driverLicenseTMod :: DriverLicenseT (B.FieldModification (B.TableField DriverLicenseT))
driverLicenseTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      documentImageId1 = B.fieldNamed "document_image_id1",
      documentImageId2 = B.fieldNamed "document_image_id2",
      driverDob = B.fieldNamed "driver_dob",
      driverName = B.fieldNamed "driver_name",
      licenseNumberEncrypted = B.fieldNamed "license_number_encrypted",
      licenseNumberHash = B.fieldNamed "license_number_hash",
      licenseExpiry = B.fieldNamed "license_expiry",
      classOfVehicles = B.fieldNamed "class_of_vehicles",
      failedRules = B.fieldNamed "failed_rules",
      verificationStatus = B.fieldNamed "verification_status",
      consent = B.fieldNamed "consent",
      consentTimestamp = B.fieldNamed "consent_timestamp",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverLicenseToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverLicenseToHSModifiers =
  M.empty

driverLicenseToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverLicenseToPSModifiers =
  M.empty

instance IsString Domain.VerificationStatus where
  fromString = show

instance Serialize DriverLicense where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverLicenseT ['id] [['driverId], ['licenseNumberHash]])
