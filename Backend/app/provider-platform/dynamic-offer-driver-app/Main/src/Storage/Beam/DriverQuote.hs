{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverQuote where

import qualified Database.Beam as B
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Common as Common
import Tools.Beam.UtilsTH

data DriverQuoteT f = DriverQuoteT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    searchTryId :: B.C f Text,
    searchRequestForDriverId :: B.C f (Maybe Text),
    estimateId :: B.C f Text,
    driverId :: B.C f Text,
    driverName :: B.C f Text,
    driverRating :: B.C f (Maybe Centesimal),
    status :: B.C f Domain.DriverQuoteStatus,
    vehicleVariant :: B.C f Variant.Variant,
    distance :: B.C f Meters,
    distanceToPickup :: B.C f Meters,
    durationToPickup :: B.C f Seconds,
    validTill :: B.C f LocalTime,
    goHomeRequestId :: B.C f (Maybe Text),
    estimatedFare :: B.C f Common.Money,
    fareParametersId :: B.C f Text,
    providerId :: B.C f Text,
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f LocalTime,
    updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverQuoteT where
  data PrimaryKey DriverQuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverQuote = DriverQuoteT Identity

$(enableKVPG ''DriverQuoteT ['id] [['driverId], ['searchTryId], ['requestId]])

$(mkTableInstancesWithTModifier ''DriverQuoteT "driver_quote" [("requestId", "search_request_id")])
