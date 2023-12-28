{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.SearchRequestForDriver where

import qualified Domain.Types.BapMetadata as DSM
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest (DriverGoHomeRequest)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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

data SearchRequestForDriver = SearchRequestForDriver
  { id :: Id SearchRequestForDriver,
    requestId :: Id DSR.SearchRequest,
    searchTryId :: Id DST.SearchTry,
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
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
    goHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    rideFrequencyScore :: Maybe Double,
    customerCancellationDues :: HighPrecMoney
  }
  deriving (Generic, Show, PrettyShow)

data SearchRequestForDriverAPIEntity = SearchRequestForDriverAPIEntity
  { searchRequestId :: Id DST.SearchTry, -- TODO: Deprecated, to be removed
    searchTryId :: Id DST.SearchTry,
    bapName :: Maybe Text,
    bapLogo :: Maybe BaseUrl,
    startTime :: UTCTime,
    searchRequestValidTill :: UTCTime,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    baseFare :: Money,
    customerExtraFee :: Maybe Money,
    fromLocation :: DSSL.SearchReqLocation,
    toLocation :: DSSL.SearchReqLocation,
    newFromLocation :: DLoc.Location,
    newToLocation :: DLoc.Location, -- we need to show all requests or last one ?
    distance :: Meters,
    driverLatLong :: LatLong,
    driverMinExtraFee :: Maybe Money,
    driverMaxExtraFee :: Maybe Money,
    rideRequestPopupDelayDuration :: Seconds,
    specialLocationTag :: Maybe Text,
    disabilityTag :: Maybe Text,
    keepHiddenForSeconds :: Seconds,
    goHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    requestedVehicleVariant :: Variant.Variant,
    isTranslated :: Bool,
    customerCancellationDues :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, PrettyShow)

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSR.SearchRequest -> DST.SearchTry -> Maybe DSM.BapMetadata -> Seconds -> Seconds -> Variant.Variant -> Bool -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata delayDuration keepHiddenForSeconds requestedVehicleVariant isTranslated =
  SearchRequestForDriverAPIEntity
    { searchRequestId = nearbyReq.searchTryId,
      searchTryId = nearbyReq.searchTryId,
      bapName = bapMetadata <&> (.name),
      bapLogo = bapMetadata <&> (.logoUrl),
      startTime = nearbyReq.startTime,
      searchRequestValidTill = nearbyReq.searchRequestValidTill,
      distanceToPickup = nearbyReq.actualDistanceToPickup,
      durationToPickup = nearbyReq.durationToPickup,
      baseFare = searchTry.baseFare,
      customerExtraFee = searchTry.customerExtraFee,
      fromLocation = convertDomainType searchRequest.fromLocation,
      toLocation = convertDomainType searchRequest.toLocation,
      newFromLocation = searchRequest.fromLocation,
      newToLocation = searchRequest.toLocation,
      distance = searchRequest.estimatedDistance,
      driverLatLong =
        LatLong
          { lat = fromMaybe 0.0 nearbyReq.lat,
            lon = fromMaybe 0.0 nearbyReq.lon
          },
      driverMinExtraFee = nearbyReq.driverMinExtraFee,
      driverMaxExtraFee = nearbyReq.driverMaxExtraFee,
      rideRequestPopupDelayDuration = delayDuration,
      specialLocationTag = searchRequest.specialLocationTag,
      disabilityTag = searchRequest.disabilityTag,
      keepHiddenForSeconds = keepHiddenForSeconds,
      goHomeRequestId = nearbyReq.goHomeRequestId,
      customerCancellationDues = nearbyReq.customerCancellationDues,
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
