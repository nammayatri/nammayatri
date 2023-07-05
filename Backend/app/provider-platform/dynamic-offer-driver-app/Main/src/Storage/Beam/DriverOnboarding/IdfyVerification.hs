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

module Storage.Beam.DriverOnboarding.IdfyVerification where

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
import qualified Domain.Types.DriverOnboarding.Image as Image
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Image.ImageType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Image.ImageType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Image.ImageType

instance FromBackendRow Postgres Image.ImageType

instance FromField Domain.ImageExtractionValidation where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.ImageExtractionValidation where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.ImageExtractionValidation

instance FromBackendRow Postgres Domain.ImageExtractionValidation

data IdfyVerificationT f = IdfyVerificationT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    documentImageId1 :: B.C f Text,
    documentImageId2 :: B.C f (Maybe Text),
    requestId :: B.C f Text,
    docType :: B.C f Image.ImageType,
    status :: B.C f Text,
    issueDateOnDoc :: B.C f (Maybe Time.UTCTime),
    driverDateOfBirth :: B.C f (Maybe Time.UTCTime),
    documentNumberEncrypted :: B.C f Text,
    documentNumberHash :: B.C f DbHash,
    imageExtractionValidation :: B.C f Domain.ImageExtractionValidation,
    idfyResponse :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IdfyVerificationT where
  data PrimaryKey IdfyVerificationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta IdfyVerificationT where
  modelFieldModification = idfyVerificationTMod
  modelTableName = "idfy_verification"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type IdfyVerification = IdfyVerificationT Identity

instance FromJSON IdfyVerification where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON IdfyVerification where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show IdfyVerification

deriving stock instance Ord Image.ImageType

deriving stock instance Ord Domain.ImageExtractionValidation

idfyVerificationTMod :: IdfyVerificationT (B.FieldModification (B.TableField IdfyVerificationT))
idfyVerificationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      documentImageId1 = B.fieldNamed "document_image_id1",
      documentImageId2 = B.fieldNamed "document_image_id2",
      requestId = B.fieldNamed "request_id",
      docType = B.fieldNamed "doc_type",
      status = B.fieldNamed "status",
      issueDateOnDoc = B.fieldNamed "issue_date_on_doc",
      driverDateOfBirth = B.fieldNamed "driver_date_of_birth",
      documentNumberEncrypted = B.fieldNamed "document_number_encrypted",
      documentNumberHash = B.fieldNamed "document_number_hash",
      imageExtractionValidation = B.fieldNamed "image_extraction_validation",
      idfyResponse = B.fieldNamed "idfy_response",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

idfyVerificationToHSModifiers :: M.Map Text (A.Value -> A.Value)
idfyVerificationToHSModifiers =
  M.empty

idfyVerificationToPSModifiers :: M.Map Text (A.Value -> A.Value)
idfyVerificationToPSModifiers =
  M.empty

instance IsString Image.ImageType where
  fromString = show

instance IsString Domain.ImageExtractionValidation where
  fromString = show

instance Serialize IdfyVerification where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''IdfyVerificationT ['id] [['driverId], ['requestId]])
