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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Driver.GoHomeFeature.DriverHomeLocation where

--import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as Domain
--import Storage.Tabular.Person (PersonTId)

import qualified Data.Aeson as A
--import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
--import Database.Beam.Backend
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
-- import Database.Beam.Postgres
--   ( Postgres,
--     ResultError (ConversionFailed, UnexpectedNull),
--   )
--import Database.PostgreSQL.Simple.FromField (FromField, fromField)
--import qualified Database.PostgreSQL.Simple.FromField as DPSF
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
--import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
--import Lib.Utils
import Sequelize

data DriverHomeLocationT f = DriverHomeLocationT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverHomeLocationT where
  data PrimaryKey DriverHomeLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverHomeLocationT where
  modelFieldModification = driverHomeLocationTMod
  modelTableName = "driver_home_location"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverHomeLocation = DriverHomeLocationT Identity

instance FromJSON DriverHomeLocation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverHomeLocation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverHomeLocation

driverHomeLocationTMod :: DriverHomeLocationT (B.FieldModification (B.TableField DriverHomeLocationT))
driverHomeLocationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      createdAt = B.fieldNamed "created_at"
    }

instance Serialize DriverHomeLocation where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverHomeLocationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverHomeLocationToHSModifiers =
  M.empty

driverHomeLocationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverHomeLocationToPSModifiers =
  M.empty

$(enableKVPG ''DriverHomeLocationT ['id] [])
