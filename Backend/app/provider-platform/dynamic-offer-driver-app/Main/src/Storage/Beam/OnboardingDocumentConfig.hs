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

module Storage.Beam.OnboardingDocumentConfig where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
-- import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.OnboardingDocumentConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Domain.VehicleClassCheckType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.VehicleClassCheckType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.VehicleClassCheckType

instance FromBackendRow Postgres Domain.VehicleClassCheckType

-- instance FromField [Text] where
--   fromField f mbValue = V.toList <$> (fromField f mbValue)

-- instance FromBackendRow Postgres [Text]

instance FromField Domain.DocumentType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DocumentType where
  sqlValueSyntax = autoSqlValueSyntax

-- instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Text] where
--   sqlValueSyntax x = sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DocumentType

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

instance FromBackendRow Postgres Domain.DocumentType

data OnboardingDocumentConfigT f = OnboardingDocumentConfigT
  { merchantId :: B.C f Text,
    documentType :: B.C f Domain.DocumentType,
    checkExtraction :: B.C f Bool,
    checkExpiry :: B.C f Bool,
    validVehicleClasses :: B.C f [Text],
    vehicleClassCheckType :: B.C f Domain.VehicleClassCheckType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OnboardingDocumentConfigT where
  data PrimaryKey OnboardingDocumentConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta OnboardingDocumentConfigT where
  modelFieldModification = onboardingDocumentConfigTMod
  modelTableName = "onboarding_document_configs"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type OnboardingDocumentConfig = OnboardingDocumentConfigT Identity

instance FromJSON OnboardingDocumentConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON OnboardingDocumentConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show OnboardingDocumentConfig

deriving stock instance Ord Domain.VehicleClassCheckType

deriving stock instance Eq Domain.VehicleClassCheckType

onboardingDocumentConfigTMod :: OnboardingDocumentConfigT (B.FieldModification (B.TableField OnboardingDocumentConfigT))
onboardingDocumentConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      documentType = B.fieldNamed "document_type",
      checkExtraction = B.fieldNamed "check_extraction",
      checkExpiry = B.fieldNamed "check_expiry",
      validVehicleClasses = B.fieldNamed "valid_vehicle_classes",
      vehicleClassCheckType = B.fieldNamed "vehicle_class_check_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

onboardingDocumentConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
onboardingDocumentConfigToHSModifiers =
  M.empty

onboardingDocumentConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
onboardingDocumentConfigToPSModifiers =
  M.empty

instance IsString Domain.DocumentType where
  fromString = show

instance IsString Domain.VehicleClassCheckType where
  fromString = show

defaultOnboardingDocumentConfig :: OnboardingDocumentConfig
defaultOnboardingDocumentConfig =
  OnboardingDocumentConfigT
    { merchantId = "",
      documentType = "",
      checkExtraction = False,
      checkExpiry = False,
      validVehicleClasses = [""],
      vehicleClassCheckType = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize OnboardingDocumentConfig where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''OnboardingDocumentConfigT ['documentType] [])
