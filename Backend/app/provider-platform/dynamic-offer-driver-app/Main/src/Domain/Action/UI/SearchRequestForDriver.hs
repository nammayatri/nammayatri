{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SearchRequestForDriver where

import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.Settlement (chargeOn)
import Control.Applicative ((<|>))
import Data.Aeson.TH
import Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.ConditionalCharges as DAC
import Domain.Types.DriverGoHomeRequest (DriverGoHomeRequest)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchReqLocation as DSSL
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.VehicleVariant as DV
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Price ()
import qualified SharedLogic.Settlement as Settlement
import SharedLogic.Type

data IOSSearchRequestForDriverAPIEntity = IOSSearchRequestForDriverAPIEntity
  { searchRequestId :: Id DST.SearchTry, -- TODO: Deprecated, to be removed
    distanceToPickup :: Meters,
    vehicleServiceTier :: Maybe Text,
    baseFare :: Money,
    searchRequestValidTill :: UTCTime,
    distanceWithUnit :: Maybe Distance,
    duration :: Maybe Seconds,
    isReferredRideReq :: Maybe Bool,
    driverCancellationNotAllowed :: Maybe Bool
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
    returnTime :: Maybe UTCTime,
    middleStopCount :: Int,
    parcelType :: Maybe DParcel.ParcelType,
    parcelQuantity :: Maybe Int,
    conditionalCharges :: [DAC.ConditionalChargesCategories],
    isSafetyPlus :: Bool,
    coinsRewardedOnGoldTierRide :: Maybe Int,
    safetyPlusCharges :: Maybe HighPrecMoney,
    commissionCharges :: Maybe HighPrecMoney,
    isPaymentOnline :: Maybe Bool,
    driverCancellationNotAllowed :: Maybe Bool,
    isScheduled :: Bool,
    -- Only present when the driver is short of the minimum wallet balance this scheduled ride needs.
    scheduledRideMinWalletBalance :: Maybe HighPrecMoney,
    customerRating :: Maybe Centesimal,
    showRoutePerRequest :: Maybe Bool,
    -- Only present when at least one of the driver's own preferences applied to this request.
    preferenceMatchScore :: Maybe Double
  }
  deriving (Generic, ToSchema, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''SearchRequestForDriverAPIEntity)

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSR.SearchRequest -> DST.SearchTry -> Maybe DSM.BapMetadata -> Seconds -> Maybe HighPrecMoney -> Seconds -> DVST.ServiceTierType -> Bool -> Bool -> Bool -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe DFP.FullFarePolicy -> Maybe Tax.Regime -> Maybe DRD.RiderDetails -> Maybe Bool -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata delayDuration mbDriverDefaultExtraForSpecialLocation keepHiddenForSeconds requestedVehicleServiceTier isTranslated isValueAddNP useSilentFCMForForwardBatch driverPickUpCharges parkingCharge safetyCharges congestionCharges petCharges priorityCharges tollCharges govtCharges mbFarePolicy mbTaxRegime mbRiderDetails showRoutePerRequest = do
  let isTollApplicable = DTC.isTollApplicableForTrip requestedVehicleServiceTier searchTry.tripCategory
      specialZoneExtraTip = (\a -> if a == 0 then Nothing else Just a) =<< min nearbyReq.driverMaxExtraFee mbDriverDefaultExtraForSpecialLocation
      driverDefaultStepFee = specialZoneExtraTip <|> nearbyReq.driverDefaultStepFee
      driverStepFee = minNonZero specialZoneExtraTip nearbyReq.driverStepFee
      isAutoPlus = requestedVehicleServiceTier == DVST.AUTO_PLUS
      taxRegime = fromMaybe Tax.GST mbTaxRegime
      netFare mbTaxConfig mbComponent = Settlement.netTaxExclusive (mbFarePolicy >>= (.commissionChargeConfig)) searchTry.paymentChargeDetails taxRegime mbComponent mbTaxConfig
      rideNet component = netFare (mbFarePolicy >>= (.rideTaxConfig)) (Just component)
      driverFeeNet = rideNet DFP.DriverSelectedFareComponent
      tollTaxConfig = mbFarePolicy >>= (.tollTaxConfig)
      parkingTaxConfig = mbFarePolicy >>= (.parkingTaxConfig)
      cCharges = roundToIntegral . rideNet DFP.CongestionChargeComponent <$> congestionCharges
      pCharges = roundToIntegral . rideNet DFP.PriorityChargeComponent <$> priorityCharges
      rawBaseFare = fromMaybe searchTry.baseFare nearbyReq.baseFare
      duesTax = (.total) <$> searchRequest.customerCancellationDuesTaxBreakup
      componentTax = fromMaybe 0 govtCharges + maybe 0 (* Tax.taxRateOf tollTaxConfig) tollCharges + maybe 0 (* Tax.taxRateOf parkingTaxConfig) parkingCharge + fromMaybe 0 duesTax
      driverPaymentCharge = searchTry.paymentChargeDetails <&> \details -> Tax.taxedTotal (chargeOn details (rawBaseFare - fromMaybe 0 searchTry.discountAmount))
      netBaseFare = Settlement.netTotalTaxInclusive taxRegime nearbyReq.commissionCharges driverPaymentCharge ((.bearer) <$> searchTry.paymentChargeDetails) componentTax 0 rawBaseFare
      customerExtraFee = rideNet DFP.CustomerExtraFeeComponent <$> searchTry.customerExtraFee
      driverMinExtraFee' = driverFeeNet <$> nearbyReq.driverMinExtraFee
      driverMaxExtraFee' = driverFeeNet <$> nearbyReq.driverMaxExtraFee
      driverPickUpCharges' = rideNet DFP.DeadKmFareComponent <$> driverPickUpCharges
      specialZoneExtraTip' = driverFeeNet <$> specialZoneExtraTip
      tollCharges' = netFare tollTaxConfig (Just DFP.TollChargesComponent) <$> tollCharges
      shownDistanceToPickup = fromMaybe nearbyReq.actualDistanceToPickup nearbyReq.displayDistanceToPickup
      shownDurationToPickup = fromMaybe nearbyReq.durationToPickup nearbyReq.displayDurationToPickup
   in SearchRequestForDriverAPIEntity
        { searchRequestId = nearbyReq.searchTryId,
          searchTryId = nearbyReq.searchTryId,
          congestionCharges = if isAutoPlus then pCharges else cCharges,
          petCharges = roundToIntegral . rideNet DFP.PetChargeComponent <$> petCharges,
          bapName = bapMetadata <&> (.name),
          bapLogo = bapMetadata >>= (.logoUrl),
          startTime = nearbyReq.startTime,
          billingCategory = searchTry.billingCategory,
          searchRequestValidTill = nearbyReq.searchRequestValidTill,
          distanceToPickup = shownDistanceToPickup,
          distanceToPickupWithUnit = convertMetersToDistance nearbyReq.distanceUnit shownDistanceToPickup,
          durationToPickup = shownDurationToPickup,
          baseFare = roundToIntegral netBaseFare, -- net fare = gross fare - commission
          baseFareWithCurrency = PriceAPIEntity (roundAmountByCurrency' nearbyReq.currency netBaseFare) nearbyReq.currency,
          customerExtraFee = roundToIntegral <$> customerExtraFee,
          customerExtraFeeWithCurrency = flip PriceAPIEntity searchTry.currency <$> customerExtraFee,
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
          driverMinExtraFee = Just $ maybe 0 roundToIntegral driverMinExtraFee',
          driverMinExtraFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> driverMinExtraFee',
          driverMaxExtraFee = Just $ maybe 0 roundToIntegral driverMaxExtraFee',
          driverMaxExtraFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> driverMaxExtraFee',
          driverDefaultStepFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency . driverFeeNet <$> nearbyReq.driverDefaultStepFee, -- TODO :: Deprecate this after UI stops consuming
          driverDefaultStepFeeWithCurrencyV2 = flip PriceAPIEntity nearbyReq.currency . driverFeeNet <$> (min nearbyReq.driverMaxExtraFee driverDefaultStepFee),
          driverStepFeeWithCurrency = flip PriceAPIEntity nearbyReq.currency . driverFeeNet <$> driverStepFee,
          rideRequestPopupDelayDuration = delayDuration,
          specialLocationTag = searchRequest.specialLocationTag,
          disabilityTag = searchRequest.disabilityTag,
          keepHiddenForSeconds = keepHiddenForSeconds,
          goHomeRequestId = nearbyReq.goHomeRequestId,
          -- customerCancellationDues = nearbyReq.customerCancellationDues,
          -- customerCancellationDuesWithCurrency = PriceAPIEntity nearbyReq.customerCancellationDues nearbyReq.currency,
          tripCategory = searchTry.tripCategory,
          isScheduled = searchTry.isScheduled,
          scheduledRideMinWalletBalance = Nothing,
          preferenceMatchScore =
            if fromMaybe False nearbyReq.hasApplicablePreferences
              then nearbyReq.preferenceMatchScore
              else Nothing,
          duration = searchRequest.estimatedDuration,
          pickupZone = nearbyReq.pickupZone,
          driverPickUpCharges = roundToIntegral <$> driverPickUpCharges',
          driverPickUpChargesWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> driverPickUpCharges',
          specialZoneExtraTip = roundToIntegral <$> specialZoneExtraTip', -- TODO :: Deprecate this after UI stops consuming
          specialZoneExtraTipWithCurrency = flip PriceAPIEntity nearbyReq.currency <$> specialZoneExtraTip', -- TODO :: Deprecate this after UI stops consuming
          vehicleServiceTier = nearbyReq.vehicleServiceTierName,
          airConditioned = nearbyReq.airConditioned,
          requestedVehicleVariant = DV.castServiceTierToVariant requestedVehicleServiceTier,
          tollCharges = tollCharges',
          tollChargesWithCurrency = flip PriceAPIEntity searchRequest.currency <$> tollCharges',
          parkingCharge = netFare parkingTaxConfig (Just DFP.ParkingChargeComponent) <$> parkingCharge,
          tollNames = if isTollApplicable then searchRequest.tollNames else Nothing,
          useSilentFCMForForwardBatch = useSilentFCMForForwardBatch && nearbyReq.isForwardRequest,
          isOnRide = nearbyReq.isForwardRequest,
          isReferredRideReq = searchRequest.driverIdForSearch $> True,
          isFavourite = nearbyReq.isFavourite,
          roundTrip = searchRequest.roundTrip,
          returnTime = searchRequest.returnTime,
          cancellationCharges = roundToIntegral . Settlement.netTaxInclusive searchTry.paymentChargeDetails taxRegime duesTax searchRequest.customerCancellationDuesCommission <$> searchRequest.customerCancellationDues,
          middleStopCount = fromMaybe 0 nearbyReq.middleStopCount,
          parcelType = nearbyReq.parcelType,
          parcelQuantity = nearbyReq.parcelQuantity,
          conditionalCharges = nearbyReq.conditionalCharges,
          isSafetyPlus = fromMaybe False nearbyReq.isSafetyPlus,
          coinsRewardedOnGoldTierRide = nearbyReq.coinsRewardedOnGoldTierRide,
          safetyPlusCharges = Just (rideNet DFP.ConditionalChargesComponent safetyCharges),
          commissionCharges = nearbyReq.commissionCharges,
          isPaymentOnline = deriveIsPaymentOnline searchTry.paymentInstrument,
          driverCancellationNotAllowed = nearbyReq.driverCancellationNotAllowed,
          customerRating = mbRiderDetails >>= (.customerRating),
          ..
        }
  where
    minNonZero Nothing b = b
    minNonZero (Just 0) b = b
    minNonZero a b = min a b
    deriveIsPaymentOnline :: Maybe DMPM.PaymentInstrument -> Maybe Bool
    deriveIsPaymentOnline = fmap (\case DMPM.Cash -> False; _ -> True)

mkSentEntityKey :: Id SearchRequestForDriver -> Text
mkSentEntityKey sReqFDId = "SRFD:SentEntity:" <> sReqFDId.getId

getSentEntity :: Redis.HedisFlow m r => SearchRequestForDriver -> m (Maybe SearchRequestForDriverAPIEntity)
getSentEntity sReqFD = Redis.withCrossAppRedis $ Redis.safeGet (mkSentEntityKey sReqFD.id)

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
