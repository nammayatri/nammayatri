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

module Storage.Beam.BusinessEvent where

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
import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle.Variant (Variant)
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

instance FromField Domain.EventType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.EventType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.EventType

instance FromBackendRow Postgres Domain.EventType

-- instance FromField Variant where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant

-- instance FromBackendRow Postgres Variant

instance FromField Domain.WhenPoolWasComputed where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.WhenPoolWasComputed where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.WhenPoolWasComputed

instance FromBackendRow Postgres Domain.WhenPoolWasComputed

data BusinessEventT f = BusinessEventT
  { id :: B.C f Text,
    driverId :: B.C f (Maybe Text),
    eventType :: B.C f Domain.EventType,
    timeStamp :: B.C f Time.UTCTime,
    bookingId :: B.C f (Maybe Text),
    whenPoolWasComputed :: B.C f (Maybe Domain.WhenPoolWasComputed),
    vehicleVariant :: B.C f (Maybe Variant),
    distance :: B.C f (Maybe Int),
    duration :: B.C f (Maybe Int),
    rideId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table BusinessEventT where
  data PrimaryKey BusinessEventT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta BusinessEventT where
  modelFieldModification = businessEventTMod
  modelTableName = "business_event"
  mkExprWithDefault _ = B.insertExpressions []

type BusinessEvent = BusinessEventT Identity

instance FromJSON BusinessEvent where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON BusinessEvent where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.EventType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.EventType where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.WhenPoolWasComputed where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.WhenPoolWasComputed where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show BusinessEvent

deriving stock instance Ord Domain.EventType

deriving stock instance Ord Domain.WhenPoolWasComputed

instance IsString Domain.EventType where
  fromString = show

instance IsString Domain.WhenPoolWasComputed where
  fromString = show

businessEventTMod :: BusinessEventT (B.FieldModification (B.TableField BusinessEventT))
businessEventTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      eventType = B.fieldNamed "event_type",
      timeStamp = B.fieldNamed "time_stamp",
      bookingId = B.fieldNamed "booking_id",
      whenPoolWasComputed = B.fieldNamed "when_pool_was_computed",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      distance = B.fieldNamed "distance",
      duration = B.fieldNamed "duration",
      rideId = B.fieldNamed "ride_id"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

businessEventToHSModifiers :: M.Map Text (A.Value -> A.Value)
businessEventToHSModifiers =
  M.empty

businessEventToPSModifiers :: M.Map Text (A.Value -> A.Value)
businessEventToPSModifiers =
  M.empty

defaultBusinessEvent :: BusinessEvent
defaultBusinessEvent =
  BusinessEventT
    { id = "",
      driverId = Nothing,
      eventType = "",
      timeStamp = defaultUTCDate,
      bookingId = Nothing,
      whenPoolWasComputed = Nothing,
      vehicleVariant = Nothing,
      distance = Nothing,
      duration = Nothing,
      rideId = Nothing
    }

instance Serialize BusinessEvent where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''BusinessEventT ['id] [])
