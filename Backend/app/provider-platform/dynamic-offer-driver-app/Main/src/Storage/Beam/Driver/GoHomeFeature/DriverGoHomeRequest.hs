{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest where

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
import qualified Database.Beam.Schema.Tables as BST
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Storage.Esqueleto (Point (..))
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.DriverGoHomeRequestStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DriverGoHomeRequestStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DriverGoHomeRequestStatus

instance FromBackendRow Postgres Domain.DriverGoHomeRequestStatus

instance IsString Domain.DriverGoHomeRequestStatus where
  fromString = show

driverGoHomeRequestTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverGoHomeRequestT)
driverGoHomeRequestTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "driver_go_home_request"
    <> B.modifyTableFields driverGoHomeRequestTMod

toRowExpression reqId driverId lat lon status numCancellation createdAt updatedAt =
  DriverGoHomeRequestT
    (B.val_ reqId)
    (B.val_ driverId)
    (B.val_ lat)
    (B.val_ lon)
    (getPoint (lat, lon))
    (B.val_ status)
    (B.val_ numCancellation)
    (B.val_ createdAt)
    (B.val_ updatedAt)

data DriverGoHomeRequestT f = DriverGoHomeRequestT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    point :: B.C f Point,
    status :: B.C f Domain.DriverGoHomeRequestStatus,
    numCancellation :: B.C f Int,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverGoHomeRequestT where
  data PrimaryKey DriverGoHomeRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverGoHomeRequestT where
  modelFieldModification = driverGoHomeRequestTMod
  modelTableName = "driver_go_home_request"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverGoHomeRequest = DriverGoHomeRequestT Identity

instance FromJSON DriverGoHomeRequest where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverGoHomeRequest where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverGoHomeRequest

driverGoHomeRequestTMod :: DriverGoHomeRequestT (B.FieldModification (B.TableField DriverGoHomeRequestT))
driverGoHomeRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      point = B.fieldNamed "point",
      status = B.fieldNamed "status",
      numCancellation = B.fieldNamed "num_cancellation",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize DriverGoHomeRequest where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverGoHomeRequestToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverGoHomeRequestToHSModifiers =
  M.empty

driverGoHomeRequestToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverGoHomeRequestToPSModifiers =
  M.empty

$(enableKVPG ''DriverGoHomeRequestT ['id] [])
