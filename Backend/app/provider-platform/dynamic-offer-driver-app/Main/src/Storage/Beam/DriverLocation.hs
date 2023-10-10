{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverLocation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as B
import qualified Database.Beam.Schema.Tables as B
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (Point (..))
import Kernel.Types.Common hiding (id)
import Lib.Schema
import Sequelize
import qualified Storage.DBModel as DBModel
import qualified Tools.Beam.UtilsTH as TH

data DriverLocationT f = DriverLocationT
  { driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    point :: B.C f Point,
    coordinatesCalculatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    merchantId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

-- FIXME we can't use templates here because of unsafePerformIO in getLocationSchemaName
driverLocationTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverLocationT)
driverLocationTable =
  B.setEntitySchema (Just getLocationSchemaName)
    <> B.setEntityName "driver_location"
    <> B.modifyTableFields driverLocationTMod

instance B.Table DriverLocationT where
  data PrimaryKey DriverLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta DriverLocationT where
  modelFieldModification = driverLocationTMod
  modelTableName = "driver_location"
  modelSchemaName = Just getLocationSchemaName

type DriverLocation = DriverLocationT Identity

deriving anyclass instance FromJSON DriverLocation

deriving anyclass instance ToJSON DriverLocation

deriving stock instance Show DriverLocation

driverLocationTMod :: DriverLocationT (B.FieldModification (B.TableField DriverLocationT))
driverLocationTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      point = B.fieldNamed "point",
      coordinatesCalculatedAt = B.fieldNamed "coordinates_calculated_at",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      merchantId = B.fieldNamed "merchant_id"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverLocationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverLocationToHSModifiers =
  M.empty

driverLocationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverLocationToPSModifiers =
  M.empty

toRowExpression :: Text -> LatLong -> UTCTime -> UTCTime -> Text -> DriverLocationT (B.QExpr B.Postgres s)
toRowExpression personId latLong updateTime now merchantId =
  DriverLocationT
    { driverId = B.val_ personId,
      lat = B.val_ latLong.lat,
      lon = B.val_ latLong.lon,
      point = getPoint (latLong.lat, latLong.lon),
      coordinatesCalculatedAt = B.val_ updateTime,
      createdAt = B.val_ now,
      updatedAt = B.val_ now,
      merchantId = B.val_ merchantId
    }

instance Serialize DriverLocation where
  put = error "undefined"
  get = error "undefined"

instance TH.IsDBTable DBModel.DriverApp DriverLocationT where -- different schema
  getDBModel _ _ = TH.DriverLocation

$(TH.enableKVPG ''DriverLocationT ['driverId] [])
