{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SearchRequestForDriver where

import Control.Applicative ((<|>))
import Data.Aeson.TH
import Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.ConditionalCharges as DAC
import Domain.Types.DriverGoHomeRequest (DriverGoHomeRequest)
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.SearchReqLocation as DSSL
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.VehicleVariant as DV
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.Type

data IOSSearchRequestForDriverAPIEntity = IOSSearchRequestForDriverAPIEntity
  { searchRequestId :: Id DST.SearchTry, -- TODO: Deprecated, to be removed
    distanceToPickup :: Meters,
    vehicleServiceTier :: Maybe Text,
    baseFare :: Money,
    searchRequestValidTill :: UTCTime,
    distanceWithUnit :: Maybe Distance,
    duration :: Maybe Seconds,
    isReferredRideReq :: Maybe Bool
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
    distanceToPickupWithUnit :: Distance,
    durationToPickup :: Seconds,
    baseFare :: Money,
    customerExtraFee :: Maybe Money,
    baseFareWithCurrency :: PriceAPIEntity,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    fromLocation :: DSSL.SearchReqLocation,
    toLocation :: Maybe DSSL.SearchReqLocation,
    -- newFromLocation :: DLoc.Location, -- in the upcoming ui release, it will be expecting both of these fields
    -- newToLocation :: Maybe DLoc.Location, -- we need to show all requests or last one ?
    distance :: Maybe Meters,
    distanceWithUnit :: Maybe Distance,
    duration :: Maybe Seconds,
    tripCategory :: DTC.TripCategory,
    driverLatLong :: LatLong,
    driverMinExtraFee :: Maybe Money,
    driverMaxExtraFee :: Maybe Money,
    congestionCharges :: Maybe Money,
    petCharges :: Maybe Money,
    cancellationCharges :: Maybe Money,
    specialZoneExtraTip :: Maybe Money,
    driverMinExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    driverMaxExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    driverDefaultStepFeeWithCurrency :: Maybe PriceAPIEntity,
    driverDefaultStepFeeWithCurrencyV2 :: Maybe PriceAPIEntity,
    driverStepFeeWithCurrency :: Maybe PriceAPIEntity,
    specialZoneExtraTipWithCurrency :: Maybe PriceAPIEntity,
    billingCategory :: BillingCategory,
    pickupZone :: Bool,
    rideRequestPopupDelayDuration :: Seconds,
    specialLocationTag :: Maybe Text,
    disabilityTag :: Maybe Text,
    keepHiddenForSeconds :: Seconds,
    goHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    requestedVehicleVariant :: DV.VehicleVariant,
    vehicleServiceTier :: Maybe Text,
    airConditioned :: Maybe Bool,
    isTranslated :: Bool,
    -- customerCancellationDues :: HighPrecMoney,   -- not used in the ui therefore removing both of these fields
    -- customerCancellationDuesWithCurrency :: PriceAPIEntity,
    isValueAddNP :: Bool,
    driverPickUpCharges :: Maybe Money,
    driverPickUpChargesWithCurrency :: Maybe PriceAPIEntity,
    tollCharges :: Maybe HighPrecMoney,
    tollChargesWithCurrency :: Maybe PriceAPIEntity,
    useSilentFCMForForwardBatch :: Bool,
    isOnRide :: Bool,
    tollNames :: Maybe [Text],
    parkingCharge :: Maybe HighPrecMoney,
    isFavourite :: Maybe Bool,
    isReferredRideReq :: Maybe Bool,
    roundTrip :: Maybe Bool,
    middleStopCount :: Int,
    parcelType :: Maybe DParcel.ParcelType,
    parcelQuantity :: Maybe Int,
    conditionalCharges :: [DAC.ConditionalChargesCategories],
    isSafetyPlus :: Bool,
    coinsRewardedOnGoldTierRide :: Maybe Int,
    safetyPlusCharges :: Maybe HighPrecMoney
  }
  deriving (Generic, ToSchema, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''SearchRequestForDriverAPIEntity)

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSR.SearchRequest -> DST.SearchTry -> Maybe DSM.BapMetadata -> Seconds -> Maybe HighPrecMoney -> Seconds -> DVST.ServiceTierType -> Bool -> Bool -> Bool -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata delayDuration mbDriverDefaultExtraForSpecialLocation keepHiddenForSeconds requestedVehicleServiceTier isTranslated isValueAddNP useSilentFCMForForwardBatch driverPickUpCharges parkingCharge safetyCharges congestionCharges petCharges priorityCharges = do
  let isTollApplicable = DTC.isTollApplicableForTrip requestedVehicleServiceTier searchTry.tripCategory
      specialZoneExtraTip = (\a -> if a == 0 then Nothing else Just a) =<< min nearbyReq.driverMaxExtraFee mbDriverDefaultExtraForSpecialLocation
      driverDefaultStepFee = specialZoneExtraTip <|> nearbyReq.driverDefaultStepFee
      driverStepFee = minNonZero specialZoneExtraTip nearbyReq.driverStepFee
      isAutoPlus = requestedVehicleServiceTier == DVST.AUTO_PLUS
      cCharges = roundToIntegral <$> congestionCharges
      pCharges = roundToIntegral <$> priorityCharges
   in SearchRequestForDriverAPIEntity
        { searchRequestId = nearbyReq.searchTryId,
          searchTryId = nearbyReq.searchTryId,
          congestionCharges = if isAutoPlus then pCharges else cCharges,
          petCharges = roundToIntegral <$> petCharges,
          bapName = bapMetadata <&> (.name),
          bapLogo = bapMetadata >>= (.logoUrl),
          startTime = nearbyReq.startTime,
          billingCategory = searchTry.billingCategory,
          searchRequestValidTill = nearbyReq.searchRequestValidTill,
          distanceToPickup = nearbyReq.actualDistanceToPickup,
          distanceToPickupWithUnit = convertMetersToDistance nearbyReq.distanceUnit nearbyReq.actualDistanceToPickup,
          durationToPickup = nearbyReq.durationToPickup,
          baseFare = roundToIntegral $ fromMaybe searchTry.baseFare nearbyReq.baseFare, -- short term, later remove searchTry.baseFare
          baseFareWithCurrency = PriceAPIEntity (fromMaybe searchTry.baseFare nearbyReq.baseFare) nearbyReq.currency,
          customerExtraFee = roundToIntegral <$> searchTry.customerExtraFee,
          customerExtraFeeWithCurrency = flip PriceAPIEntity searchTry.currency <$> searchTry.customerExtraFee,
          fromLocation = convertDomainType searchRequest.fromLocation,
          toLocation = convertDomainType <$> searchRequest.toLocation,
          -- newFromLocation = searchRequest.fromLocation,
          -- newToLocation = searchRequest.toLocation,
          distance = searchRequest.estimatedDistance,
          distanceWithUnit = convertMetersToDistance searchRequest.distanceUnit <$> searchRequest.estimatedDistance,
          driverLatLong =
            LatLong
              { lat = fromMaybe 0.0 nearbyReq.lat,
                lon = fromMaybe 0.0 nearbyReq.lon
              },
          driverMinExtraFee = Just $ maybe 0 roundToIntegral nearbyReq.driverMinExtraFee,
          driverMinExtraFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> nearbyReq.driverMinExtraFee,
          driverMaxExtraFee = Just $ maybe 0 roundToIntegral nearbyReq.driverMaxExtraFee,
          driverMaxExtraFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> nearbyReq.driverMaxExtraFee,
          driverDefaultStepFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> nearbyReq.driverDefaultStepFee, -- TODO :: Deprecate this after UI stops consuming
          driverDefaultStepFeeWithCurrencyV2 = flip PriceAPIEntity nearbyReq.currency <$> (min nearbyReq.driverMaxExtraFee driverDefaultStepFee),
          driverStepFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> driverStepFee,
          rideRequestPopupDelayDuration = delayDuration,
          specialLocationTag = searchRequest.specialLocationTag,
          disabilityTag = searchRequest.disabilityTag,
          keepHiddenForSeconds = keepHiddenForSeconds,
          goHomeRequestId = nearbyReq.goHomeRequestId,
          -- customerCancellationDues = nearbyReq.customerCancellationDues,
          -- customerCancellationDuesWithCurrency = PriceAPIEntity nearbyReq.customerCancellationDues nearbyReq.currency,
          tripCategory = searchTry.tripCategory,
          duration = searchRequest.estimatedDuration,
          pickupZone = nearbyReq.pickupZone,
          driverPickUpCharges = roundToIntegral <$> driverPickUpCharges,
          driverPickUpChargesWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> driverPickUpCharges,
          specialZoneExtraTip = roundToIntegral <$> specialZoneExtraTip, -- TODO :: Deprecate this after UI stops consuming
          specialZoneExtraTipWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> specialZoneExtraTip, -- TODO :: Deprecate this after UI stops consuming
          vehicleServiceTier = nearbyReq.vehicleServiceTierName,
          airConditioned = nearbyReq.airConditioned,
          requestedVehicleVariant = DV.castServiceTierToVariant requestedVehicleServiceTier,
          tollCharges = if isTollApplicable then searchRequest.tollCharges else Nothing,
          tollChargesWithCurrency = flip PriceAPIEntity searchRequest.currency <$> if isTollApplicable then searchRequest.tollCharges else Nothing,
          tollNames = if isTollApplicable then searchRequest.tollNames else Nothing,
          useSilentFCMForForwardBatch = useSilentFCMForForwardBatch,
          isOnRide = nearbyReq.isForwardRequest,
          isReferredRideReq = searchRequest.driverIdForSearch $> True,
          isFavourite = nearbyReq.isFavourite,
          roundTrip = searchRequest.roundTrip,
          cancellationCharges = roundToIntegral <$> searchRequest.customerCancellationDues,
          middleStopCount = fromMaybe 0 nearbyReq.middleStopCount,
          parcelType = nearbyReq.parcelType,
          parcelQuantity = nearbyReq.parcelQuantity,
          conditionalCharges = nearbyReq.conditionalCharges,
          isSafetyPlus = fromMaybe False nearbyReq.isSafetyPlus,
          coinsRewardedOnGoldTierRide = nearbyReq.coinsRewardedOnGoldTierRide,
          safetyPlusCharges = Just safetyCharges,
          ..
        }
  where
    minNonZero Nothing b = b
    minNonZero (Just 0) b = b
    minNonZero a b = min a b

extractDriverPickupCharges :: DFP.FarePolicyDetailsD s -> Maybe HighPrecMoney
extractDriverPickupCharges farePolicyDetails =
  case farePolicyDetails of
    DFP.ProgressiveDetails det -> Just det.deadKmFare
    DFP.RentalDetails det -> Just det.deadKmFare
    DFP.InterCityDetails det -> Just det.deadKmFare
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
      instructions = address.instructions,
      extras = address.extras,
      merchantOperatingCityId = merchantOperatingCityId,
      ..
    }
