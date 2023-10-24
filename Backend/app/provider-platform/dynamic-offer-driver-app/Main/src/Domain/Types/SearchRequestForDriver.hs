{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchRequestForDriver where

import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.Booking as DB
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest (DriverGoHomeRequest)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSSL
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data DriverSearchRequestStatus = Active | Inactive
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (PrettyShow) via Showable DriverSearchRequestStatus

$(mkBeamInstancesForEnum ''DriverSearchRequestStatus)

data SearchRequestForDriverResponse
  = Accept
  | Reject
  | Pulled
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable SearchRequestForDriverResponse

$(mkBeamInstancesForEnum ''SearchRequestForDriverResponse)

-- rename RideRequestForDriver or RideRequest?
data SearchRequestForDriver = SearchRequestForDriver
  { id :: Id SearchRequestForDriver,
    requestId :: Id DSR.SearchRequest,
    searchRequestTag :: DSR.SearchRequestTag,
    searchTryId :: Id DST.SearchTry,
    bookingId :: Maybe (Id DB.Booking), -- only for rental case
    merchantId :: Maybe (Id DM.Merchant),
    startTime :: UTCTime,
    searchRequestValidTill :: UTCTime,
    driverId :: Id Person,
    actualDistanceToPickup :: Meters,
    straightLineDistanceToPickup :: Meters,
    durationToPickup :: Seconds,
    vehicleVariant :: Variant.Variant,
    status :: DriverSearchRequestStatus,
    batchNumber :: Int,
    lat :: Maybe Double,
    lon :: Maybe Double,
    createdAt :: UTCTime,
    response :: Maybe SearchRequestForDriverResponse,
    driverMinExtraFee :: Maybe Money,
    driverMaxExtraFee :: Maybe Money,
    rideRequestPopupDelayDuration :: Seconds,
    isPartOfIntelligentPool :: Bool,
    cancellationRatio :: Maybe Double,
    acceptanceRatio :: Maybe Double,
    driverAvailableTime :: Maybe Double,
    parallelSearchRequestCount :: Maybe Int,
    driverSpeed :: Maybe Double,
    keepHiddenForSeconds :: Seconds,
    mode :: Maybe DI.DriverMode,
    goHomeRequestId :: Maybe (Id DriverGoHomeRequest)
  }
  deriving (Generic, Show, PrettyShow)

data SearchRequestForDriverAPIEntity = SearchRequestForDriverAPIEntity
  { searchRequestId :: Id DST.SearchTry, -- TODO: Deprecated, to be removed
    searchTryId :: Id DST.SearchTry,
    searchRequestTag :: DSR.SearchRequestTag,
    bapName :: Maybe Text,
    bapLogo :: Maybe BaseUrl,
    startTime :: UTCTime,
    searchRequestValidTill :: UTCTime,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    baseFare :: Money,
    customerExtraFee :: Maybe Money,
    fromLocation :: DSSL.SearchReqLocation,
    toLocation :: Maybe DSSL.SearchReqLocation,
    newFromLocation :: DLoc.Location,
    newToLocation :: Maybe DLoc.Location, -- we need to show all requests or last one ?
    distance :: Meters,
    duration :: Maybe Seconds, -- only for rental case (baseDuration)
    driverLatLong :: LatLong,
    driverMinExtraFee :: Maybe Money,
    driverMaxExtraFee :: Maybe Money,
    rideRequestPopupDelayDuration :: Seconds,
    specialLocationTag :: Maybe Text,
    disabilityTag :: Maybe Text,
    keepHiddenForSeconds :: Seconds,
    goHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    requestedVehicleVariant :: Variant.Variant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, PrettyShow)

data SearchDetails
  = OnDemandDetails OnDemandSearchDetails
  | RentalDetails RentalSearchDetails

data OnDemandSearchDetails = OnDemandSearchDetails
  { searchTry :: DST.SearchTry,
    searchReqDetails :: DSR.SearchRequestDetailsOnDemand
  }

data RentalSearchDetails = RentalSearchDetails
  { searchTry :: DST.SearchTry,
    booking :: DB.Booking
  }

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSR.SearchRequest -> SearchDetails -> Maybe DSM.BapMetadata -> Seconds -> Seconds -> Variant.Variant -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchDetails bapMetadata delayDuration keepHiddenForSeconds requestedVehicleVariant = do
  let (searchRequestTag, baseFare, customerExtraFee, distance, duration, newFromLocation, newToLocation, specialLocationTag) = case searchDetails of
        OnDemandDetails OnDemandSearchDetails {searchTry, searchReqDetails} -> do
          let distance' = searchReqDetails.estimatedDistance
              newFromLocation' = searchReqDetails.fromLocation
              newToLocation' = Just $ searchReqDetails.toLocation
              specialLocationTag' = searchReqDetails.specialLocationTag
          (DSR.ON_DEMAND, searchTry.baseFare, searchTry.customerExtraFee, distance', Nothing, newFromLocation', newToLocation', specialLocationTag')
        RentalDetails RentalSearchDetails {booking} -> do
          let distance' = booking.estimatedDistance
              newFromLocation' = booking.fromLocation
          (DSR.RENTAL, booking.estimatedFare, Nothing, distance', Just booking.estimatedDuration, newFromLocation', Nothing, Nothing)
  SearchRequestForDriverAPIEntity
    { searchRequestId = nearbyReq.searchTryId,
      searchTryId = nearbyReq.searchTryId,
      searchRequestTag,
      bapName = bapMetadata <&> (.name),
      bapLogo = bapMetadata <&> (.logoUrl),
      startTime = nearbyReq.startTime,
      searchRequestValidTill = nearbyReq.searchRequestValidTill,
      distanceToPickup = nearbyReq.actualDistanceToPickup,
      durationToPickup = nearbyReq.durationToPickup,
      baseFare = baseFare,
      customerExtraFee,
      fromLocation = convertDomainType newFromLocation,
      toLocation = convertDomainType <$> newToLocation,
      newFromLocation,
      newToLocation,
      distance,
      driverLatLong =
        LatLong
          { lat = fromMaybe 0.0 nearbyReq.lat,
            lon = fromMaybe 0.0 nearbyReq.lon
          },
      driverMinExtraFee = nearbyReq.driverMinExtraFee,
      driverMaxExtraFee = nearbyReq.driverMaxExtraFee,
      rideRequestPopupDelayDuration = delayDuration,
      specialLocationTag,
      disabilityTag = searchRequest.disabilityTag,
      keepHiddenForSeconds = keepHiddenForSeconds,
      goHomeRequestId = nearbyReq.goHomeRequestId,
      ..
    }

convertDomainType :: DLoc.Location -> DSSL.SearchReqLocation
convertDomainType DLoc.Location {..} =
  DSSL.SearchReqLocation
    { id = cast id,
      street = address.street,
      door = address.door,
      city = address.city,
      state = address.state,
      country = address.country,
      building = address.building,
      areaCode = address.areaCode,
      area = address.area,
      full_address = address.fullAddress,
      ..
    }
