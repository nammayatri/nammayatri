{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking.API where

import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Booking.BookingLocation (BookingLocationAPIEntity)
import qualified Domain.Types.Booking.BookingLocation as SLoc
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.RentalSlab as DRentalSlab
import Domain.Types.Ride (Ride, RideAPIEntity, makeRideAPIEntity)
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    status :: BookingStatus,
    agencyName :: Text,
    agencyNumber :: Text,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    fromLocation :: BookingLocationAPIEntity,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: BookingAPIDetails,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    duration :: Maybe Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingAPIDetails
  = OneWayAPIDetails OneWayBookingAPIDetails
  | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  | DriverOfferAPIDetails OneWayBookingAPIDetails
  | OneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON BookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

data OneWayBookingAPIDetails = OneWayBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity,
    estimatedDistance :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWaySpecialZoneBookingAPIDetails = OneWaySpecialZoneBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity,
    estimatedDistance :: HighPrecMeters,
    otpCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeBookingAPIEntity :: Booking -> Maybe Ride -> [Ride] -> [FareBreakup] -> BookingAPIEntity
makeBookingAPIEntity booking activeRide allRides fareBreakups = do
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails
  BookingAPIEntity
    { id = booking.id,
      status = booking.status,
      agencyName = booking.providerName,
      agencyNumber = booking.providerMobileNumber,
      estimatedFare = booking.estimatedFare,
      discount = booking.discount,
      estimatedTotalFare = booking.estimatedTotalFare,
      fromLocation = SLoc.makeBookingLocationAPIEntity booking.fromLocation,
      rideList = allRides <&> makeRideAPIEntity,
      tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
      fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
      bookingDetails,
      rideStartTime = activeRide >>= (.rideStartTime),
      rideEndTime = activeRide >>= (.rideEndTime),
      duration = getRideDuration activeRide,
      createdAt = booking.createdAt,
      updatedAt = booking.updatedAt
    }
  where
    getRideDuration :: Maybe DRide.Ride -> Maybe Seconds
    getRideDuration mbRide = do
      ride <- mbRide
      startTime <- ride.rideStartTime
      endTime <- ride.rideEndTime
      return $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime

    mkBookingAPIDetails :: BookingDetails -> BookingAPIDetails
    mkBookingAPIDetails = \case
      OneWayDetails details -> OneWayAPIDetails . mkOneWayAPIDetails $ details
      RentalDetails DRentalSlab.RentalSlab {..} -> RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
      DriverOfferDetails details -> DriverOfferAPIDetails . mkOneWayAPIDetails $ details
      OneWaySpecialZoneDetails details -> OneWaySpecialZoneAPIDetails . mkOneWaySpecialZoneAPIDetails $ details
      where
        mkOneWayAPIDetails OneWayBookingDetails {..} =
          OneWayBookingAPIDetails
            { toLocation = SLoc.makeBookingLocationAPIEntity toLocation,
              estimatedDistance = distance
            }
        mkOneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingDetails {..} =
          OneWaySpecialZoneBookingAPIDetails
            { toLocation = SLoc.makeBookingLocationAPIEntity toLocation,
              estimatedDistance = distance,
              ..
            }

buildBookingAPIEntity :: EsqDBReplicaFlow m r => Booking -> m BookingAPIEntity
buildBookingAPIEntity booking = do
  mbRide <- runInReplica $ QRide.findActiveByRBId booking.id
  rideList <- runInReplica $ QRide.findAllByRBId booking.id
  fareBreakups <- runInReplica $ QFareBreakup.findAllByBookingId booking.id
  return $ makeBookingAPIEntity booking mbRide rideList fareBreakups
