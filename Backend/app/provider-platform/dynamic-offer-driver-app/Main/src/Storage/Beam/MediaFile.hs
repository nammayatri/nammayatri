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

module Storage.Beam.MediaFile where

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
import qualified Domain.Types.MediaFile as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

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

data MediaFileT f = MediaFileT
  { id :: B.C f Text,
    fileType :: B.C f Domain.MediaType,
    url :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MediaFileT where
  data PrimaryKey MediaFileT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta MediaFileT where
  modelFieldModification = mediaFileTMod
  modelTableName = "media_file"
  mkExprWithDefault _ = B.insertExpressions []

type MediaFile = MediaFileT Identity

instance FromJSON MediaFile where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MediaFile where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MediaFile

deriving stock instance Ord Domain.MediaType

deriving stock instance Eq Domain.MediaType

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.MediaType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.MediaType where
  sqlValueSyntax = autoSqlValueSyntax

mediaFileTMod :: MediaFileT (B.FieldModification (B.TableField MediaFileT))
mediaFileTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      fileType = B.fieldNamed "file_type",
      url = B.fieldNamed "url",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

mediaFileToHSModifiers :: M.Map Text (A.Value -> A.Value)
mediaFileToHSModifiers =
  M.fromList
    []

mediaFileToPSModifiers :: M.Map Text (A.Value -> A.Value)
mediaFileToPSModifiers =
  M.fromList
    []

instance IsString Domain.MediaType where
  fromString = show

defaultMediaFile :: MediaFile
defaultMediaFile =
  MediaFileT
    { id = "",
      fileType = "",
      url = "",
      createdAt = defaultDate
    }

instance Serialize MediaFile where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''MediaFileT ['id] [])
