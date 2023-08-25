{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.BookingCancellationReason where

import qualified Data.Aeson.Types as A
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Booking.Type (Booking)
import Domain.Types.CancellationReason (CancellationReasonCode, CancellationStage)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Ride (Ride)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data BookingCancellationReason = BookingCancellationReason
  { bookingId :: Id Booking,
    rideId :: Maybe (Id Ride),
    merchantId :: Maybe (Id DM.Merchant),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    reasonStage :: Maybe CancellationStage,
    additionalInfo :: Maybe Text,
    driverCancellationLocation :: Maybe LatLong,
    driverDistToPickup :: Maybe Meters
  }
  deriving (Generic, Show)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving (Show, Eq, Ord, Read, Generic)

instance FromField CancellationSource where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CancellationSource where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be CancellationSource

instance FromBackendRow Postgres CancellationSource

instance IsString CancellationSource where
  fromString = show

instance FromJSON CancellationSource where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON CancellationSource where
  toJSON = A.genericToJSON A.defaultOptions
