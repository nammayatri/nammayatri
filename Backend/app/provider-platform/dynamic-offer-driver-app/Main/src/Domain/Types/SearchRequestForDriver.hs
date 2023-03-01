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

import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data DriverSearchRequestStatus = Active | Inactive
  deriving (Show, Read, Eq)
  deriving (PrettyShow) via Showable DriverSearchRequestStatus

data SearchRequestForDriverResponse
  = Accept
  | Reject
  | Pulled
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
    customerExtraFee :: Maybe Money,
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money,
    rideRequestPopupDelayDuration :: Seconds,
    isPartOfIntelligentPool :: Bool,
    cancellationRatio :: Maybe Double,
    acceptanceRatio :: Maybe Double,
    driverAvailableTime :: Maybe Double,
    parallelSearchRequestCount :: Maybe Int
  }
  deriving (Generic, Show, PrettyShow)

data SearchRequestForDriverAPIEntity = SearchRequestForDriverAPIEntity
  { searchRequestId :: Id SearchRequest,
    startTime :: UTCTime,
    searchRequestValidTill :: UTCTime,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    baseFare :: Money,
    customerExtraFee :: Maybe Money,
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
      customerExtraFee = nearbyReq.customerExtraFee,
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
