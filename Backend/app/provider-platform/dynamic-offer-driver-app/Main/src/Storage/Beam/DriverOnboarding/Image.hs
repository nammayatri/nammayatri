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

module Storage.Beam.DriverOnboarding.Image where

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
import qualified Domain.Types.DriverOnboarding.Error as Domain
import qualified Domain.Types.DriverOnboarding.Image as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Domain.ImageType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.ImageType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.ImageType

instance FromBackendRow Postgres Domain.ImageType

instance IsString Domain.ImageType where
  fromString = show

instance FromField Domain.DriverOnboardingError where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DriverOnboardingError where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DriverOnboardingError

instance FromBackendRow Postgres Domain.DriverOnboardingError

instance IsString Domain.DriverOnboardingError where
  fromString = show

data ImageT f = ImageT
  { id :: B.C f Text,
    personId :: B.C f Text,
    merchantId :: B.C f Text,
    s3Path :: B.C f Text,
    imageType :: B.C f Domain.ImageType,
    isValid :: B.C f Bool,
    failureReason :: B.C f (Maybe Domain.DriverOnboardingError),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ImageT where
  data PrimaryKey ImageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta ImageT where
  modelFieldModification = imageTMod
  modelTableName = "image"
  mkExprWithDefault _ = B.insertExpressions []

type Image = ImageT Identity

instance FromJSON Image where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Image where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Image

deriving stock instance Ord Domain.ImageType

deriving stock instance Ord Domain.DriverOnboardingError

imageTMod :: ImageT (B.FieldModification (B.TableField ImageT))
imageTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      personId = B.fieldNamed "person_id",
      merchantId = B.fieldNamed "merchant_id",
      s3Path = B.fieldNamed "s3_path",
      imageType = B.fieldNamed "image_type",
      isValid = B.fieldNamed "is_valid",
      failureReason = B.fieldNamed "failure_reason",
      createdAt = B.fieldNamed "created_at"
    }

defaultImage :: Image
defaultImage =
  ImageT
    { id = "",
      personId = "",
      merchantId = "",
      s3Path = "",
      imageType = "",
      isValid = False,
      failureReason = Nothing,
      createdAt = defaultUTCDate
    }

instance Serialize Image where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

imageToHSModifiers :: M.Map Text (A.Value -> A.Value)
imageToHSModifiers =
  M.empty

imageToPSModifiers :: M.Map Text (A.Value -> A.Value)
imageToPSModifiers =
  M.empty

$(enableKVPG ''ImageT ['id] [])
