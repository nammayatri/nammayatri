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
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.OnboardingDocumentConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Domain.VehicleClassCheckType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.VehicleClassCheckType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.VehicleClassCheckType

instance FromBackendRow Postgres Domain.VehicleClassCheckType

instance FromField Domain.DocumentType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DocumentType where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [Text] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DocumentType

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

instance FromBackendRow Postgres Domain.DocumentType

data OnboardingDocumentConfigT f = OnboardingDocumentConfigT
  { merchantId :: B.C f Text,
    documentType :: B.C f Domain.DocumentType,
    checkExtraction :: B.C f Bool,
    checkExpiry :: B.C f Bool,
    validVehicleClasses :: B.C f [Text],
    vehicleClassCheckType :: B.C f Domain.VehicleClassCheckType,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
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
  mkExprWithDefault _ = B.insertExpressions []

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
  M.fromList
    []

onboardingDocumentConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
onboardingDocumentConfigToPSModifiers =
  M.fromList
    []

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
      createdAt = defaultDate,
      updatedAt = defaultDate
    }

instance Serialize OnboardingDocumentConfig where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''OnboardingDocumentConfigT ['documentType] [])
