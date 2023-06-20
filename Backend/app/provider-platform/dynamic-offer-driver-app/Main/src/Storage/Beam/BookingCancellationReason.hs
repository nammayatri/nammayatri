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

module Storage.Beam.BookingCancellationReason where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.BookingCancellationReason as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Domain.CancellationSource where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.CancellationSource where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.CancellationSource

instance FromBackendRow Postgres Domain.CancellationSource

data BookingCancellationReasonT f = BookingCancellationReasonT
  { driverId :: B.C f (Maybe Text),
    bookingId :: B.C f Text,
    rideId :: B.C f (Maybe Text),
    source :: B.C f Domain.CancellationSource,
    reasonCode :: B.C f (Maybe Text),
    additionalInfo :: B.C f (Maybe Text),
    driverCancellationLocationLat :: B.C f (Maybe Double),
    driverCancellationLocationLon :: B.C f (Maybe Double),
    driverDistToPickup :: B.C f (Maybe Meters)
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.CancellationSource where
  fromString = show

instance B.Table BookingCancellationReasonT where
  data PrimaryKey BookingCancellationReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . bookingId

instance ModelMeta BookingCancellationReasonT where
  modelFieldModification = bookingCancellationReasonTMod
  modelTableName = "booking_cancellation_reason"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type BookingCancellationReason = BookingCancellationReasonT Identity

instance FromJSON BookingCancellationReason where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON BookingCancellationReason where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.CancellationSource where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.CancellationSource where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show BookingCancellationReason

bookingCancellationReasonTMod :: BookingCancellationReasonT (B.FieldModification (B.TableField BookingCancellationReasonT))
bookingCancellationReasonTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      bookingId = B.fieldNamed "booking_id",
      rideId = B.fieldNamed "ride_id",
      source = B.fieldNamed "source",
      reasonCode = B.fieldNamed "reason_code",
      additionalInfo = B.fieldNamed "additional_info",
      driverCancellationLocationLat = B.fieldNamed "driver_cancellation_location_lat",
      driverCancellationLocationLon = B.fieldNamed "driver_cancellation_location_lon",
      driverDistToPickup = B.fieldNamed "driver_dist_to_pickup"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

bookingCancellationReasonToHSModifiers :: M.Map Text (A.Value -> A.Value)
bookingCancellationReasonToHSModifiers =
  M.empty

bookingCancellationReasonToPSModifiers :: M.Map Text (A.Value -> A.Value)
bookingCancellationReasonToPSModifiers =
  M.empty

defaultBookingCancellationReason :: BookingCancellationReason
defaultBookingCancellationReason =
  BookingCancellationReasonT
    { driverId = Nothing,
      bookingId = "",
      rideId = Nothing,
      source = "",
      reasonCode = Nothing,
      additionalInfo = Nothing,
      driverCancellationLocationLat = Nothing,
      driverCancellationLocationLon = Nothing,
      driverDistToPickup = Nothing
    }

instance Serialize BookingCancellationReason where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''BookingCancellationReasonT ['bookingId] [['rideId], ['driverId]])
