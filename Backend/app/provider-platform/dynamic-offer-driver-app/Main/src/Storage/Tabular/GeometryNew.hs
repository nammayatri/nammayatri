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

module Storage.Tabular.GeometryNew where

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
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
-- import qualified Database.Beam.Schema.Tables as DBST

import qualified Data.Text as T
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
import qualified Debug.Trace as T
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Vehicle as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (KVConnector (..), MeshConfig (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import qualified EulerHS.Language as L
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

-- data A = A | B
--   deriving stock (Eq, Generic, Read, Show, Ord)
--   deriving anyclass (A.FromJSON, A.ToJSON)

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

-- instance FromField Domain.BookingStatus where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingStatus where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingStatus

-- instance FromBackendRow Postgres Domain.BookingStatus

-- instance FromField Domain.BookingType where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingType where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingType

-- instance FromBackendRow Postgres Domain.BookingType

-- -- deriving stock instance Read GeoRestriction

-- instance FromField Veh.Variant where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Veh.Variant where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Veh.Variant

-- instance FromBackendRow Postgres Veh.Variant

-- instance FromField Meters where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters

-- instance FromField Centesimal where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

-- instance FromBackendRow Postgres Centesimal

--

-- instance FromField Money where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

-- instance FromBackendRow Postgres Money

-- instance FromField Seconds where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

-- instance FromBackendRow Postgres Seconds

data GeometryNewT f = GeometryNewT
  { id :: B.C f Text,
    region :: B.C f Text
  }
  -- data GeometryNewT f = GeometryNew {
  --     id :: B.C f Text
  --   }
  deriving (Generic, B.Beamable)

instance B.Table GeometryNewT where
  data PrimaryKey GeometryNewT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta GeometryNewT where
  modelFieldModification = bookingTMod
  modelTableName = "geometry"
  modelSchemaName = Just "atlas_driver_offer_bpp"
  mkExprWithDefault _ = B.insertExpressions []

type GeometryNew = GeometryNewT Identity

instance FromJSON GeometryNew where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON GeometryNew where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show GeometryNew

bookingTMod :: GeometryNewT (B.FieldModification (B.TableField GeometryNewT))
bookingTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      region = B.fieldNamed "region"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

geometryNewToHSModifiers :: M.Map Text (A.Value -> A.Value)
geometryNewToHSModifiers =
  M.fromList
    []

geometryNewToPSModifiers :: M.Map Text (A.Value -> A.Value)
geometryNewToPSModifiers =
  M.fromList
    []

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

findById :: (L.MonadFlow m) => Text -> m (Maybe GeometryNew)
findById bookingId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> T.trace (T.unpack bookingId) $ either (\x -> T.trace (show x) Nothing) (\x -> x) <$> KV.findWithKVConnector dbCOnf' meshConfig [Is id $ Eq bookingId]
    Nothing -> T.trace "Rahull Nothing" $ pure Nothing

-- instance Serialize Domain.BookingStatus where

instance Serialize GeometryNew where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''GeometryNewT ['id] [])
