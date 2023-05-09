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
{-# OPTIONS_GHC -Wwarn=type-defaults #-}

module Storage.Tabular.BookingNew where

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
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
-- import qualified Database.Beam.Schema.Tables as DBST
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Booking as Domain
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

data A = A | B
  deriving stock (Eq, Generic, Read, Show, Ord)
  deriving anyclass (A.FromJSON, A.ToJSON)

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

instance FromField Domain.BookingStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingStatus

instance FromBackendRow Postgres Domain.BookingStatus

instance FromField Domain.BookingType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingType

instance FromBackendRow Postgres Domain.BookingType

-- deriving stock instance Read GeoRestriction

instance FromField Veh.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Veh.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Veh.Variant

instance FromBackendRow Postgres Veh.Variant

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

deriving stock instance Read Money

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

data BookingNewT f = BookingNew
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    quoteId :: B.C f Text,
    status :: B.C f Text,
    bookingType :: B.C f Text,
    specialZoneOtpCode :: B.C f (Maybe Text),
    providerId :: B.C f Text,
    primaryExophone :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    startTime :: B.C f Time.LocalTime,
    riderId :: B.C f (Maybe Text),
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    vehicleVariant :: B.C f Veh.Variant,
    estimatedDistance :: B.C f Meters,
    maxEstimatedDistance :: B.C f (Maybe Centesimal),
    estimatedFare :: B.C f Money,
    estimatedDuration :: B.C f Seconds,
    fareParametersId :: B.C f Int,
    riderName :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  -- data BookingNewT f = BookingNew {
  --     id :: B.C f Text
  --   }
  deriving (Generic, B.Beamable)

instance B.Table BookingNewT where
  data PrimaryKey BookingNewT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta BookingNewT where
  modelFieldModification = bookingTMod
  modelTableName = "booking"
  mkExprWithDefault _ = B.insertExpressions []

type BookingNew = BookingNewT Identity

instance FromJSON BookingNew where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON BookingNew where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show BookingNew

bookingTMod :: BookingNewT (B.FieldModification (B.TableField BookingNewT))
bookingTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "id",
      quoteId = B.fieldNamed "id",
      status = B.fieldNamed "id",
      bookingType = B.fieldNamed "id",
      specialZoneOtpCode = B.fieldNamed "id",
      providerId = B.fieldNamed "id",
      primaryExophone = B.fieldNamed "id",
      bapId = B.fieldNamed "id",
      bapUri = B.fieldNamed "id",
      startTime = B.fieldNamed "id",
      riderId = B.fieldNamed "id",
      fromLocationId = B.fieldNamed "id",
      toLocationId = B.fieldNamed "id",
      vehicleVariant = B.fieldNamed "id",
      estimatedDistance = B.fieldNamed "id",
      maxEstimatedDistance = B.fieldNamed "id",
      estimatedFare = B.fieldNamed "id",
      estimatedDuration = B.fieldNamed "id",
      fareParametersId = B.fieldNamed "id",
      riderName = B.fieldNamed "id",
      createdAt = B.fieldNamed "id",
      updatedAt = B.fieldNamed "id"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

bookingNewToHSModifiers :: M.Map Text (A.Value -> A.Value)
bookingNewToHSModifiers =
  M.fromList
    []

bookingNewToPSModifiers :: M.Map Text (A.Value -> A.Value)
bookingNewToPSModifiers =
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
      kvHardKilled = False,
      cerealEnabled = False
    }

findById :: (L.MonadFlow m) => Text -> m (Maybe BookingNew)
findById bookingId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (const Nothing) (\x -> x) <$> KV.findWithKVConnector dbCOnf' meshConfig ([And [Is id (Eq bookingId)]])
    Nothing -> pure Nothing

-- instance Serialize Domain.BookingStatus where

instance Serialize BookingNew where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''BookingNewT ['id] [])
