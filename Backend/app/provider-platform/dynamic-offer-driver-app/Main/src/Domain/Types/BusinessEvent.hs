{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.BusinessEvent where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Booking (Booking)
import Domain.Types.Person (Driver)
import Domain.Types.Ride (Ride)
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data BusinessEvent = BusinessEvent
  { id :: Id BusinessEvent,
    driverId :: Maybe (Id Driver),
    eventType :: EventType,
    timeStamp :: UTCTime,
    bookingId :: Maybe (Id Booking),
    whenPoolWasComputed :: Maybe WhenPoolWasComputed,
    vehicleVariant :: Maybe Variant,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    rideId :: Maybe (Id Ride)
  }
  deriving (Generic)

data EventType = DRIVER_IN_POOL | RIDE_COMMENCED | DRIVER_ASSIGNED | RIDE_CONFIRMED
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance FromField EventType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EventType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be EventType

instance FromBackendRow Postgres EventType

instance IsString EventType where
  fromString = show

data WhenPoolWasComputed = ON_SEARCH | ON_CONFIRM | ON_REALLOCATION
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance FromField WhenPoolWasComputed where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be WhenPoolWasComputed where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be WhenPoolWasComputed

instance FromBackendRow Postgres WhenPoolWasComputed

instance IsString WhenPoolWasComputed where
  fromString = show
