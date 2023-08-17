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

module Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import Domain.Types.Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
-- import Kernel.Types.Common hiding (id)

import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

-- instance FromField Domain.VerificationStatus where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.VerificationStatus where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.VerificationStatus

-- instance FromBackendRow Postgres Domain.VerificationStatus

-- instance IsString Domain.VerificationStatus where
--   fromString = show

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { id :: B.C f Text,
    documentImageId :: B.C f Text,
    certificateNumberEncrypted :: B.C f Text,
    certificateNumberHash :: B.C f DbHash,
    fitnessExpiry :: B.C f Time.UTCTime,
    permitExpiry :: B.C f (Maybe Time.UTCTime),
    pucExpiry :: B.C f (Maybe Time.UTCTime),
    insuranceValidity :: B.C f (Maybe Time.UTCTime),
    vehicleClass :: B.C f (Maybe Text),
    vehicleVariant :: B.C f (Maybe Variant),
    vehicleManufacturer :: B.C f (Maybe Text),
    vehicleCapacity :: B.C f (Maybe Int),
    vehicleModel :: B.C f (Maybe Text),
    vehicleColor :: B.C f (Maybe Text),
    vehicleEnergyType :: B.C f (Maybe Text),
    verificationStatus :: B.C f Domain.VerificationStatus,
    failedRules :: B.C f [Text],
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleRegistrationCertificateT where
  data PrimaryKey VehicleRegistrationCertificateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type VehicleRegistrationCertificate = VehicleRegistrationCertificateT Identity

deriving stock instance Ord Domain.VerificationStatus

vehicleRegistrationCertificateTMod :: VehicleRegistrationCertificateT (B.FieldModification (B.TableField VehicleRegistrationCertificateT))
vehicleRegistrationCertificateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      documentImageId = B.fieldNamed "document_image_id",
      certificateNumberEncrypted = B.fieldNamed "certificate_number_encrypted",
      certificateNumberHash = B.fieldNamed "certificate_number_hash",
      fitnessExpiry = B.fieldNamed "fitness_expiry",
      permitExpiry = B.fieldNamed "permit_expiry",
      pucExpiry = B.fieldNamed "puc_expiry",
      insuranceValidity = B.fieldNamed "insurance_validity",
      vehicleClass = B.fieldNamed "vehicle_class",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      vehicleManufacturer = B.fieldNamed "vehicle_manufacturer",
      vehicleCapacity = B.fieldNamed "vehicle_capacity",
      vehicleModel = B.fieldNamed "vehicle_model",
      vehicleColor = B.fieldNamed "vehicle_color",
      vehicleEnergyType = B.fieldNamed "vehicle_energy_type",
      verificationStatus = B.fieldNamed "verification_status",
      failedRules = B.fieldNamed "failed_rules",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''VehicleRegistrationCertificateT ['id] [['certificateNumberHash]])

$(mkTableInstances ''VehicleRegistrationCertificateT "vehicle_registration_certificate" "atlas_driver_offer_bpp")
