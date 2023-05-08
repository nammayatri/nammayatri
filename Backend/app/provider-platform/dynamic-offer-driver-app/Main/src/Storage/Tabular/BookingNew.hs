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

module Storage.Tabular.BookingNew where

import qualified Database.Beam as B
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
-- import Lib.UtilsTH

import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Sequelize
import Storage.Tabular.Booking.BookingLocation hiding (createdAt, id, updatedAt)
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.Vehicle ()

-- import           EulerHS.KVConnector.Types (KVConnector(..), MeshMeta(..), tableName, primaryKey, secondaryKeys)

data BookingNewT f = BookingNew
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    quoteId :: B.C f Text,
    status :: B.C f Domain.BookingStatus,
    bookingType :: B.C f Domain.BookingType,
    specialZoneOtpCode :: B.C f (Maybe Text),
    providerId :: B.C f MerchantTId,
    primaryExophone :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    startTime :: B.C f UTCTime,
    riderId :: B.C f (Maybe RiderDetailsTId),
    fromLocationId :: B.C f BookingLocationTId,
    toLocationId :: B.C f BookingLocationTId,
    vehicleVariant :: B.C f Veh.Variant,
    estimatedDistance :: B.C f Meters,
    maxEstimatedDistance :: B.C f (Maybe Centesimal),
    estimatedFare :: B.C f Money,
    estimatedDuration :: B.C f Seconds,
    fareParametersId :: B.C f Fare.FareParametersTId,
    riderName :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
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

data Hello = Hello
  { ids :: Text
  }

-- $(enableKV ''BookingNewT ['ids] [])
