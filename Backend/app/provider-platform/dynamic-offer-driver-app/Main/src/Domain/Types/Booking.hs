{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Data.Time.Clock.POSIX as Time
import Domain.Types.FareParameters (FareParameters)
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationMapping as DLocationMapping
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Servant.API

data BookingStatus
  = NEW
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Booking = Booking
  { id :: Id Booking,
    transactionId :: Text,
    quoteId :: Text,
    status :: BookingStatus,
    bookingType :: BookingType,
    specialZoneOtpCode :: Maybe Text,
    providerId :: Id DM.Merchant, -- FIXME merchantId
    primaryExophone :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLocation.Location,
    toLocation :: [DLocation.Location],
    vehicleVariant :: DVeh.Variant,
    estimatedDistance :: Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedFare :: Money,
    estimatedDuration :: Seconds,
    fareParams :: FareParameters,
    riderName :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data BookingType = SpecialZoneBooking | NormalBooking
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data BookingTable = BookingTable
  { id :: Id BookingTable,
    transactionId :: Text,
    quoteId :: Text,
    status :: BookingStatus,
    bookingType :: BookingType,
    specialZoneOtpCode :: Maybe Text,
    providerId :: Id DM.Merchant,
    primaryExophone :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    vehicleVariant :: DVeh.Variant,
    estimatedDistance :: Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedFare :: Money,
    estimatedDuration :: Seconds,
    fareParametersId :: Id FareParameters,
    riderName :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

locationIdGenerator :: (MonadFlow m) => (Int, DLocation.Location) -> m DLocation.Location
locationIdGenerator bookingWithIndex = do
  id <- generateGUID
  let booking = snd bookingWithIndex
  pure $
    DLocation.Location
      { lat = booking.lat,
        lon = booking.lon,
        address = booking.address,
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt,
        ..
      }

locationMappingMakerForBooking :: (MonadFlow m) => Booking -> m [DLocationMapping.LocationMapping]
locationMappingMakerForBooking booking = do
  fromId <- generateGUID
  let bookingWithIndexes = zip ([1 ..] :: [Int]) booking.toLocation
  toLocation <- mapM locationIdGenerator bookingWithIndexes
  let newBooking =
        Booking
          { id = booking.id,
            transactionId = booking.transactionId,
            quoteId = booking.quoteId,
            status = booking.status,
            bookingType = booking.bookingType,
            specialZoneOtpCode = booking.specialZoneOtpCode,
            providerId = booking.providerId,
            primaryExophone = booking.primaryExophone,
            bapId = booking.bapId,
            bapUri = booking.bapUri,
            startTime = booking.startTime,
            riderId = booking.riderId,
            fromLocation =
              DLocation.Location
                { id = fromId,
                  lat = booking.fromLocation.lat,
                  lon = booking.fromLocation.lon,
                  address = booking.fromLocation.address,
                  createdAt = booking.fromLocation.createdAt,
                  updatedAt = booking.fromLocation.updatedAt
                },
            vehicleVariant = booking.vehicleVariant,
            estimatedDistance = booking.estimatedDistance,
            maxEstimatedDistance = booking.maxEstimatedDistance,
            estimatedFare = booking.estimatedFare,
            estimatedDuration = booking.estimatedDuration,
            fareParams = booking.fareParams,
            riderName = booking.riderName,
            createdAt = booking.createdAt,
            updatedAt = booking.updatedAt,
            ..
          }
  let newbookingWithIndexes = zip ([1 ..] :: [Int]) newBooking.toLocation
  toLocationMappers <- mapM (locationMappingMakerForBookingInstanceMaker newBooking) newbookingWithIndexes
  fromLocationMapping <- locationMappingMakerForBookingInstanceMaker newBooking (0, newBooking.fromLocation)
  return $ fromLocationMapping : toLocationMappers

locationMappingMakerForBookingInstanceMaker :: (MonadFlow m) => Booking -> (Int, DLocation.Location) -> m DLocationMapping.LocationMapping
locationMappingMakerForBookingInstanceMaker Booking {..} location = do
  locationMappingId <- generateGUID
  let getIntEpochTime = round `fmap` Time.getPOSIXTime
  epochVersion <- liftIO getIntEpochTime
  let epochVersionLast5Digits = epochVersion `mod` 100000 :: Integer
  let locationMapping =
        DLocationMapping.LocationMapping
          { id = Id locationMappingId,
            tag = DLocationMapping.Booking,
            order = fst location,
            version = show epochVersionLast5Digits,
            tagId = getId id,
            location = snd location,
            ..
          }
  return locationMapping
