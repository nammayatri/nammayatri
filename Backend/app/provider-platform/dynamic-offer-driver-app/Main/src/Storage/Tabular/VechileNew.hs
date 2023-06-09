{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Storage.Tabular.VechileNew where

-- import Lib.UtilsTH

-- import Kernel.Types.Centesimal (Centesimal)
-- import Kernel.Types.Geofencing (GeoRestriction(..))

-- import Storage.Tabular.Booking.BookingLocation hiding (createdAt, id, updatedAt)
-- import qualified Storage.Tabular.FareParameters as Fare
-- import Storage.Tabular.Merchant (MerchantTId)
-- import Storage.Tabular.RiderDetails (RiderDetailsTId)

-- import qualified Database.Beam.Postgres as DBP
-- import           Database.Beam.Schema.Tables (DatabaseEntity, EntityModification)

import qualified Data.Aeson as A
-- import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
-- import qualified Database.Beam.Schema.Tables as DBST

import qualified Data.Text as T
import Data.Time
-- import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Debug.Trace as T
-- import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Vehicle as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (KVConnector (..), MeshConfig (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import qualified EulerHS.Language as L
import GHC.Generics (Generic)
-- import Kernel.Types.Common hiding (id)

import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Domain.RegistrationCategory where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.RegistrationCategory where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.RegistrationCategory

instance FromBackendRow Postgres Domain.RegistrationCategory

instance FromField Domain.Category where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Category where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Category

instance FromBackendRow Postgres Domain.Category

deriving stock instance Ord Domain.Category

deriving stock instance Ord Domain.RegistrationCategory

data VechileNewT f = VechileNew
  { driverId :: B.C f Text,
    merchantId :: B.C f Text,
    variant :: B.C f Veh.Variant,
    model :: B.C f Text,
    color :: B.C f Text,
    registrationNo :: B.C f Text,
    capacity :: B.C f (Maybe Int),
    category :: B.C f (Maybe Domain.Category),
    make :: B.C f (Maybe Text),
    size :: B.C f (Maybe Text),
    energyType :: B.C f (Maybe Text),
    registrationCategory :: B.C f (Maybe Domain.RegistrationCategory),
    vehicleClass :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  -- data VechileNewT f = VechileNew {
  --     id :: B.C f Text
  --   }
  deriving (Generic, B.Beamable)

instance B.Table VechileNewT where
  data PrimaryKey VechileNewT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta VechileNewT where
  modelFieldModification = bookingTMod
  modelTableName = "vehicle"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type VechileNew = VechileNewT Identity

instance FromJSON VechileNew where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON VechileNew where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show VechileNew

bookingTMod :: VechileNewT (B.FieldModification (B.TableField VechileNewT))
bookingTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      merchantId = B.fieldNamed "merchant_id",
      variant = B.fieldNamed "variant",
      model = B.fieldNamed "model",
      color = B.fieldNamed "color",
      registrationNo = B.fieldNamed "registration_no",
      capacity = B.fieldNamed "capacity",
      category = B.fieldNamed "category",
      make = B.fieldNamed "make",
      size = B.fieldNamed "size",
      energyType = B.fieldNamed "energy_type",
      registrationCategory = B.fieldNamed "registration_category",
      vehicleClass = B.fieldNamed "vehicle_class",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

vechileNewToHSModifiers :: M.Map Text (A.Value -> A.Value)
vechileNewToHSModifiers =
  M.empty

vechileNewToPSModifiers :: M.Map Text (A.Value -> A.Value)
vechileNewToPSModifiers =
  M.empty

meshConfig :: MeshConfig
meshConfig =
  MeshConfig
    { meshEnabled = False,
      memcacheEnabled = False,
      meshDBName = "ECRDB",
      ecRedisDBStream = "db-sync-stream",
      kvRedis = "KVRedis",
      redisTtl = 43200,
      kvHardKilled = True,
      cerealEnabled = False
    }

findById :: (L.MonadFlow m) => Text -> m (Maybe VechileNew)
findById bookingId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> T.trace (T.unpack bookingId) $ either (\x -> T.trace (show x) Nothing) (\x -> x) <$> KV.findWithKVConnector dbCOnf' meshConfig [Is driverId $ Eq bookingId]
    Nothing -> T.trace "Rahull Nothing" $ pure Nothing

-- instance Serialize Domain.BookingStatus where

instance Serialize VechileNew where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''VechileNewT ['driverId] [])
