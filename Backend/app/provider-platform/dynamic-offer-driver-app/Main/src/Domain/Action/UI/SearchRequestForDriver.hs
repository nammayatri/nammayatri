{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SearchRequestForDriver where

import qualified Domain.Types.BapMetadata as DSM
import Domain.Types.Common as DTC
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest (DriverGoHomeRequest)
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSSL
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as Variant
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data IOSSearchRequestForDriverAPIEntity = IOSSearchRequestForDriverAPIEntity
  { searchRequestId :: Id DST.SearchTry, -- TODO: Deprecated, to be removed
    distanceToPickup :: Meters,
    vehicleServiceTier :: Maybe Text,
    baseFare :: Money
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

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
    baseFareWithCurrency :: PriceAPIEntity,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    fromLocation :: DSSL.SearchReqLocation,
    toLocation :: Maybe DSSL.SearchReqLocation,
    newFromLocation :: DLoc.Location,
    newToLocation :: Maybe DLoc.Location, -- we need to show all requests or last one ?
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    tripCategory :: DTC.TripCategory,
    driverLatLong :: LatLong,
    driverMinExtraFee :: Maybe Money,
    driverMaxExtraFee :: Maybe Money,
    specialZoneExtraTip :: Maybe Money,
    driverMinExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    driverMaxExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    specialZoneExtraTipWithCurrency :: Maybe PriceAPIEntity,
    pickupZone :: Bool,
    rideRequestPopupDelayDuration :: Seconds,
    specialLocationTag :: Maybe Text,
    disabilityTag :: Maybe Text,
    keepHiddenForSeconds :: Seconds,
    goHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    requestedVehicleVariant :: Variant.Variant,
    vehicleServiceTier :: Maybe Text,
    airConditioned :: Maybe Bool,
    isTranslated :: Bool,
    customerCancellationDues :: HighPrecMoney,
    customerCancellationDuesWithCurrency :: PriceAPIEntity,
    isValueAddNP :: Bool,
    driverPickUpCharges :: Maybe Money,
    driverPickUpChargesWithCurrency :: Maybe PriceAPIEntity,
    tollCharges :: Maybe HighPrecMoney,
    tollChargesWithCurrency :: Maybe PriceAPIEntity,
    tollNames :: Maybe [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSR.SearchRequest -> DST.SearchTry -> Maybe DSM.BapMetadata -> Seconds -> Maybe HighPrecMoney -> Seconds -> DVST.ServiceTierType -> Bool -> Bool -> Maybe HighPrecMoney -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata delayDuration mbDriverDefaultExtraForSpecialLocation keepHiddenForSeconds requestedVehicleServiceTier isTranslated isValueAddNP driverPickUpCharges =
  let isTollApplicableForServiceTier = DTC.isTollApplicable requestedVehicleServiceTier
      specialZoneExtraTip = min nearbyReq.driverMaxExtraFee mbDriverDefaultExtraForSpecialLocation
   in SearchRequestForDriverAPIEntity
        { searchRequestId = nearbyReq.searchTryId,
          searchTryId = nearbyReq.searchTryId,
          bapName = bapMetadata <&> (.name),
          bapLogo = bapMetadata <&> (.logoUrl),
          startTime = nearbyReq.startTime,
          searchRequestValidTill = nearbyReq.searchRequestValidTill,
          distanceToPickup = nearbyReq.actualDistanceToPickup,
          durationToPickup = nearbyReq.durationToPickup,
          baseFare = roundToIntegral $ fromMaybe searchTry.baseFare nearbyReq.baseFare, -- short term, later remove searchTry.baseFare
          baseFareWithCurrency = PriceAPIEntity (fromMaybe searchTry.baseFare nearbyReq.baseFare) nearbyReq.currency,
          customerExtraFee = roundToIntegral <$> searchTry.customerExtraFee,
          customerExtraFeeWithCurrency = flip PriceAPIEntity searchTry.currency <$> searchTry.customerExtraFee,
          fromLocation = convertDomainType searchRequest.fromLocation,
          toLocation = convertDomainType <$> searchRequest.toLocation,
          newFromLocation = searchRequest.fromLocation,
          newToLocation = searchRequest.toLocation,
          distance = searchRequest.estimatedDistance,
          driverLatLong =
            LatLong
              { lat = fromMaybe 0.0 nearbyReq.lat,
                lon = fromMaybe 0.0 nearbyReq.lon
              },
          driverMinExtraFee = roundToIntegral <$> nearbyReq.driverMinExtraFee,
          driverMinExtraFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> nearbyReq.driverMinExtraFee,
          driverMaxExtraFee = roundToIntegral <$> nearbyReq.driverMaxExtraFee,
          driverMaxExtraFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> nearbyReq.driverMaxExtraFee,
          rideRequestPopupDelayDuration = delayDuration,
          specialLocationTag = searchRequest.specialLocationTag,
          disabilityTag = searchRequest.disabilityTag,
          keepHiddenForSeconds = keepHiddenForSeconds,
          goHomeRequestId = nearbyReq.goHomeRequestId,
          customerCancellationDues = nearbyReq.customerCancellationDues,
          customerCancellationDuesWithCurrency = PriceAPIEntity nearbyReq.customerCancellationDues nearbyReq.currency,
          tripCategory = searchTry.tripCategory,
          duration = searchRequest.estimatedDuration,
          pickupZone = nearbyReq.pickupZone,
          driverPickUpCharges = roundToIntegral <$> driverPickUpCharges,
          driverPickUpChargesWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> driverPickUpCharges,
          specialZoneExtraTip = roundToIntegral <$> specialZoneExtraTip,
          specialZoneExtraTipWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> specialZoneExtraTip,
          vehicleServiceTier = nearbyReq.vehicleServiceTierName,
          airConditioned = nearbyReq.airConditioned,
          requestedVehicleVariant = castServiceTierToVariant requestedVehicleServiceTier,
          tollCharges = if isTollApplicableForServiceTier then searchRequest.tollCharges else Nothing,
          tollChargesWithCurrency = flip PriceAPIEntity searchRequest.currency <$> if isTollApplicableForServiceTier then searchRequest.tollCharges else Nothing,
          tollNames = if isTollApplicableForServiceTier then searchRequest.tollNames else Nothing,
          ..
        }

extractDriverPickupCharges :: DFP.FarePolicyDetailsD s -> Maybe HighPrecMoney
extractDriverPickupCharges farePolicyDetails =
  case farePolicyDetails of
    DFP.ProgressiveDetails det -> Just det.deadKmFare
    _ -> Nothing

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
