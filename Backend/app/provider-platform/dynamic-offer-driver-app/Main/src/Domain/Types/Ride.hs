{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.Time.Clock.POSIX as Time
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationMapping as DLocationMapping
import qualified Domain.Types.Person as DPers
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types
import qualified Kernel.Prelude as BP
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.API

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, BP.ToParamSchema)

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Ride = Ride
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    otp :: Text,
    trackingUrl :: BaseUrl,
    fare :: Maybe Money,
    traveledDistance :: HighPrecMeters,
    chargeableDistance :: Maybe Meters,
    driverArrivalTime :: Maybe UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    tripStartPos :: Maybe LatLong,
    tripEndPos :: Maybe LatLong,
    fromLocation :: Maybe DLocation.Location,
    toLocation :: [DLocation.Location],
    fareParametersId :: Maybe (Id DFare.FareParameters),
    distanceCalculationFailed :: Maybe Bool,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data RideTable = RideTable
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    otp :: Text,
    trackingUrl :: BaseUrl,
    fare :: Maybe Money,
    traveledDistance :: HighPrecMeters,
    chargeableDistance :: Maybe Meters,
    driverArrivalTime :: Maybe UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    tripStartPos :: Maybe LatLong,
    tripEndPos :: Maybe LatLong,
    fareParametersId :: Maybe (Id DFare.FareParameters),
    distanceCalculationFailed :: Maybe Bool,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

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

locationMappingMakerForRide :: (MonadFlow m) => Ride -> m [DLocationMapping.LocationMapping]
locationMappingMakerForRide ride = do
  let rideWithIndexes = zip ([1 ..] :: [Int]) ride.toLocation
  toLocationMappers <- mapM (locationMappingMakerForRideInstanceMaker ride) rideWithIndexes
  case ride.fromLocation of
    Just location -> do
      fromLocationMapping <- locationMappingMakerForRideInstanceMaker ride (0, location)
      return $ fromLocationMapping : toLocationMappers
    Nothing -> return toLocationMappers

locationMappingMakerForRideInstanceMaker :: (MonadFlow m) => Ride -> (Int, DLocation.Location) -> m DLocationMapping.LocationMapping
locationMappingMakerForRideInstanceMaker Ride {..} location = do
  locationMappingId <- generateGUID
  let getIntEpochTime = round `fmap` Time.getPOSIXTime
  epochVersion <- liftIO getIntEpochTime
  let epochVersionLast5Digits = epochVersion `mod` 100000 :: Integer
  let locationMapping =
        DLocationMapping.LocationMapping
          { id = Id locationMappingId,
            tag = DLocationMapping.Ride,
            order = fst location,
            version = show epochVersionLast5Digits,
            tagId = getId id,
            location = snd location,
            ..
          }
  return locationMapping
