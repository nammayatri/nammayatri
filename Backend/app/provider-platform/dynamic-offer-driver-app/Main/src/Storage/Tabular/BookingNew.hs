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

-- import qualified Domain.Types.Booking as Domain
-- import qualified Domain.Types.Vehicle.Variant as Veh
-- import Lib.UtilsTH

-- import Kernel.Types.Common hiding (id)

-- import Storage.Tabular.Booking.BookingLocation hiding (createdAt, id, updatedAt)
-- import qualified Storage.Tabular.FareParameters as Fare
-- import Storage.Tabular.Merchant (MerchantTId)
-- import Storage.Tabular.RiderDetails (RiderDetailsTId)

-- import qualified Database.Beam.Backend as DBB
-- import qualified Database.Beam.Postgres as DBP
-- import           Database.Beam.Schema.Tables (DatabaseEntity, EntityModification)
-- import qualified Database.Beam.Schema.Tables as DBST

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

-- import           Database.PostgreSQL.Simple.FromField(FromField, fromField)
-- import qualified Database.PostgreSQL.Simple.FromField as DPSF

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
    vehicleVariant :: B.C f Text,
    estimatedDistance :: B.C f Int,
    maxEstimatedDistance :: B.C f (Maybe Int),
    estimatedFare :: B.C f Int,
    estimatedDuration :: B.C f Int,
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

$(enableKV ''BookingNewT ['id] [])
