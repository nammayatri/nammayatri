{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchRequestForDriver where

import Beckn.External.Maps.Google.PolyLinePoints
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle.Variant as Variant

data DriverSearchRequestStatus = Active | Inactive
  deriving (Show, Read, Eq)
  deriving (PrettyShow) via Showable DriverSearchRequestStatus

data SearchRequestForDriverResponse
  = Accept
  | Reject
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Read, Eq)
  deriving (PrettyShow) via Showable SearchRequestForDriverResponse

data SearchRequestForDriver = SearchRequestForDriver
  { id :: Id SearchRequestForDriver,
    searchRequestId :: Id SearchRequest,
    startTime :: UTCTime,
    searchRequestValidTill :: UTCTime,
    driverId :: Id Person,
    actualDistanceToPickup :: Meters,
    straightLineDistanceToPickup :: Meters,
    durationToPickup :: Seconds,
    vehicleVariant :: Variant.Variant,
    status :: DriverSearchRequestStatus,
    baseFare :: Money,
    batchNumber :: Int,
    lat :: Maybe Double,
    lon :: Maybe Double,
    createdAt :: UTCTime,
    response :: Maybe SearchRequestForDriverResponse,
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money,
    rideRequestPopupDelayDuration :: Seconds,
    isPartOfIntelligentPool :: Bool,
    cancellationRatio :: Maybe Double
  }
  deriving (Generic, Show, PrettyShow)

data SearchRequestForDriverAPIEntity = SearchRequestForDriverAPIEntity
  { searchRequestId :: Id SearchRequest,
    startTime :: UTCTime,
    searchRequestValidTill :: UTCTime,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    baseFare :: Money,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    distance :: Meters,
    driverLatLong :: LatLong,
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money,
    rideRequestPopupDelayDuration :: Seconds
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, PrettyShow)

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSReq.SearchRequest -> Seconds -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest delayDuration =
  SearchRequestForDriverAPIEntity
    { searchRequestId = searchRequest.id,
      startTime = nearbyReq.startTime,
      searchRequestValidTill = nearbyReq.searchRequestValidTill,
      distanceToPickup = nearbyReq.actualDistanceToPickup,
      durationToPickup = nearbyReq.durationToPickup,
      baseFare = nearbyReq.baseFare,
      fromLocation = searchRequest.fromLocation,
      toLocation = searchRequest.toLocation,
      distance = searchRequest.estimatedDistance,
      driverLatLong =
        LatLong
          { lat = fromMaybe 0.0 nearbyReq.lat,
            lon = fromMaybe 0.0 nearbyReq.lon
          },
      driverMinExtraFee = nearbyReq.driverMinExtraFee,
      driverMaxExtraFee = nearbyReq.driverMaxExtraFee,
      rideRequestPopupDelayDuration = delayDuration
    }
