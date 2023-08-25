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
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Beam.Booking.BookingLocation where

import Data.ByteString.Internal (ByteString, unpackChars)
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
  Just value' ->
    case readMaybe (unpackChars value') of
      Just val -> pure val
      _ -> DPSF.returnError DPSF.ConversionFailed f "Could not 'read' value for 'Rule'."

data BookingLocationT f = BookingLocationT
  { id :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    street :: B.C f (Maybe Text),
    door :: B.C f (Maybe Text),
    city :: B.C f (Maybe Text),
    state :: B.C f (Maybe Text),
    country :: B.C f (Maybe Text),
    building :: B.C f (Maybe Text),
    areaCode :: B.C f (Maybe Text),
    area :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingLocationT where
  data PrimaryKey BookingLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type BookingLocation = BookingLocationT Identity

bookingLocationTMod :: BookingLocationT (B.FieldModification (B.TableField BookingLocationT))
bookingLocationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      street = B.fieldNamed "street",
      door = B.fieldNamed "door",
      city = B.fieldNamed "city",
      state = B.fieldNamed "state",
      country = B.fieldNamed "country",
      building = B.fieldNamed "building",
      areaCode = B.fieldNamed "area_code",
      area = B.fieldNamed "area",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''BookingLocationT ['id] [])

$(mkTableInstances ''BookingLocationT "booking_location" "atlas_driver_offer_bpp")
