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
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Price ()
import qualified SharedLogic.ComponentCharge as SCC
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
    baseFareBreakup :: Maybe Tax.AmountBreakup,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    customerExtraFeeBreakup :: Maybe Tax.AmountBreakup,
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
    driverMinExtraFeeBreakup :: Maybe Tax.AmountBreakup,
    driverMaxExtraFee :: Maybe Money,
    driverMaxExtraFeeBreakup :: Maybe Tax.AmountBreakup,
    congestionCharges :: Maybe Money,
    congestionChargesBreakup :: Maybe Tax.AmountBreakup,
    petCharges :: Maybe Money,
    petChargesBreakup :: Maybe Tax.AmountBreakup,
    cancellationCharges :: Maybe Money,
    cancellationChargesBreakup :: Maybe Tax.AmountBreakup,
    specialZoneExtraTip :: Maybe Money,
    specialZoneExtraTipBreakup :: Maybe Tax.AmountBreakup,
    driverMinExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    driverMaxExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    driverDefaultStepFeeWithCurrency :: Maybe PriceAPIEntity,
    driverDefaultStepFeeWithCurrencyV2 :: Maybe PriceAPIEntity,
    driverDefaultStepFeeBreakup :: Maybe Tax.AmountBreakup,
    driverStepFeeWithCurrency :: Maybe PriceAPIEntity,
    driverStepFeeBreakup :: Maybe Tax.AmountBreakup,
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
    driverPickUpChargesBreakup :: Maybe Tax.AmountBreakup,
    tollCharges :: Maybe HighPrecMoney,
    tollChargesWithCurrency :: Maybe PriceAPIEntity,
    tollChargesBreakup :: Maybe Tax.AmountBreakup,
    useSilentFCMForForwardBatch :: Bool,
    isOnRide :: Bool,
    tollNames :: Maybe [Text],
    parkingCharge :: Maybe HighPrecMoney,
    parkingChargeBreakup :: Maybe Tax.AmountBreakup,
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
    safetyPlusChargesBreakup :: Maybe Tax.AmountBreakup,
    commissionCharges :: Maybe HighPrecMoney,
    isPaymentOnline :: Maybe Bool,
    driverCancellationNotAllowed :: Maybe Bool,
    isScheduled :: Bool,
    customerRating :: Maybe Centesimal,
    showRoutePerRequest :: Maybe Bool,
    -- Only present when at least one of the driver's own preferences applied to this request.
    preferenceMatchScore :: Maybe Double
  }
  deriving (Generic, ToSchema, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''SearchRequestForDriverAPIEntity)

makeSearchRequestForDriverAPIEntity :: SearchRequestForDriver -> DSR.SearchRequest -> DST.SearchTry -> Maybe DSM.BapMetadata -> Seconds -> Maybe HighPrecMoney -> Seconds -> DVST.ServiceTierType -> Bool -> Bool -> Bool -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe DFP.FareChargeConfig -> Maybe Tax.RideTax -> Maybe HighPrecMoney -> Maybe DRD.RiderDetails -> Maybe Bool -> SearchRequestForDriverAPIEntity
makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata delayDuration mbDriverDefaultExtraForSpecialLocation keepHiddenForSeconds requestedVehicleServiceTier isTranslated isValueAddNP useSilentFCMForForwardBatch driverPickUpCharges parkingCharge safetyCharges congestionCharges petCharges priorityCharges tollCharges commissionChargeConfig rideTax riderTaxes mbRiderDetails showRoutePerRequest = do
  let isTollApplicable = DTC.isTollApplicableForTrip requestedVehicleServiceTier searchTry.tripCategory
      specialZoneExtraTip = (\a -> if a == 0 then Nothing else Just a) =<< min nearbyReq.driverMaxExtraFee mbDriverDefaultExtraForSpecialLocation
      driverDefaultStepFee = specialZoneExtraTip <|> nearbyReq.driverDefaultStepFee
      driverStepFee = minNonZero specialZoneExtraTip nearbyReq.driverStepFee
      isAutoPlus = requestedVehicleServiceTier == DVST.AUTO_PLUS
      ratesIn rateCurrency = SCC.FareChargeRates {currency = rateCurrency, commissionChargeConfig = commissionChargeConfig, rideTax = rideTax, paymentChargeDetails = searchTry.paymentChargeDetails}
      rates = ratesIn nearbyReq.currency
      chargeConfig = SCC.componentChargeConfig rates . Just
      additionConfig = chargeConfig DFP.DriverSelectedFareComponent
      rawBaseFare = fromMaybe searchTry.baseFare nearbyReq.baseFare
      baseFareConfig = chargeConfig DFP.RideFare
      base = SCC.mkInclusiveNetAmount baseFareConfig SCC.IncludedCharges {tax = fromMaybe 0 riderTaxes, commission = fromMaybe 0 nearbyReq.commissionCharges, paymentCharge = Just (SCC.wholeFarePaymentCharge baseFareConfig rawBaseFare)} rawBaseFare
      congestionAmount = if isAutoPlus then priorityCharges else congestionCharges
      congestionComponent = if isAutoPlus then DFP.PriorityChargeComponent else DFP.CongestionChargeComponent
      congestion = SCC.mkNetAmount (chargeConfig congestionComponent) <$> congestionAmount
      pet = SCC.mkNetAmount (chargeConfig DFP.PetChargeComponent) <$> petCharges
      customerExtraFee = SCC.mkNetAmount (SCC.componentChargeConfig (ratesIn searchTry.currency) (Just DFP.CustomerExtraFeeComponent)) <$> searchTry.customerExtraFee
      minExtraFee = SCC.mkNetAmount additionConfig <$> nearbyReq.driverMinExtraFee
      maxExtraFee = SCC.mkNetAmount additionConfig <$> nearbyReq.driverMaxExtraFee
      deprecatedDefaultStepFee = SCC.mkNetAmount additionConfig <$> nearbyReq.driverDefaultStepFee
      defaultStepFee = SCC.mkNetAmount additionConfig <$> min nearbyReq.driverMaxExtraFee driverDefaultStepFee
      stepFee = SCC.mkNetAmount additionConfig <$> driverStepFee
      zoneExtraTip = SCC.mkNetAmount additionConfig <$> specialZoneExtraTip
      pickUp = SCC.mkNetAmount (chargeConfig DFP.DeadKmFareComponent) <$> driverPickUpCharges
      toll = SCC.mkNetAmount (chargeConfig DFP.TollChargesComponent) <$> tollCharges
      parking = SCC.mkNetAmount (chargeConfig DFP.ParkingChargeComponent) <$> parkingCharge
      safetyPlus = SCC.mkNetAmount (SCC.componentChargeConfig rates Nothing) safetyCharges
      cancellation =
        ( \dues ->
            SCC.mkInclusiveNetAmount
              (chargeConfig DFP.CustomerCancellationChargeComponent)
              SCC.IncludedCharges {tax = Tax.taxTotal searchRequest.customerCancellationDuesTaxBreakup, commission = fromMaybe 0 searchRequest.customerCancellationDuesCommission, paymentCharge = Nothing}
              dues
        )
          <$> searchRequest.customerCancellationDues
      shownDistanceToPickup = fromMaybe nearbyReq.actualDistanceToPickup nearbyReq.displayDistanceToPickup
      shownDurationToPickup = fromMaybe nearbyReq.durationToPickup nearbyReq.displayDurationToPickup
   in SearchRequestForDriverAPIEntity
        { searchRequestId = nearbyReq.searchTryId,
          searchTryId = nearbyReq.searchTryId,
          congestionCharges = roundToIntegral . fst <$> congestion,
          congestionChargesBreakup = snd <$> congestion,
          petCharges = roundToIntegral . fst <$> pet,
          petChargesBreakup = snd <$> pet,
          bapName = bapMetadata <&> (.name),
          bapLogo = bapMetadata >>= (.logoUrl),
          startTime = nearbyReq.startTime,
          billingCategory = searchTry.billingCategory,
          searchRequestValidTill = nearbyReq.searchRequestValidTill,
          distanceToPickup = shownDistanceToPickup,
          distanceToPickupWithUnit = convertMetersToDistance nearbyReq.distanceUnit shownDistanceToPickup,
          durationToPickup = shownDurationToPickup,
          baseFare = roundToIntegral (fst base),
          baseFareWithCurrency = netPrice base,
          baseFareBreakup = Just (snd base),
          customerExtraFee = roundToIntegral . fst <$> customerExtraFee,
          customerExtraFeeBreakup = snd <$> customerExtraFee,
          customerExtraFeeWithCurrency = flip PriceAPIEntity searchTry.currency . fst <$> customerExtraFee,
          fromLocation = convertDomainType searchRequest.fromLocation,
          toLocation = convertDomainType <$> searchRequest.toLocation,
          distance = searchRequest.estimatedDistance,
          distanceWithUnit = convertMetersToDistance searchRequest.distanceUnit <$> searchRequest.estimatedDistance,
          driverLatLong =
            LatLong
              { lat = fromMaybe 0.0 nearbyReq.lat,
                lon = fromMaybe 0.0 nearbyReq.lon
              },
          driverMinExtraFee = Just $ maybe 0 (roundToIntegral . fst) minExtraFee,
          driverMinExtraFeeBreakup = snd <$> minExtraFee,
          driverMinExtraFeeWithCurrency = netPrice <$> minExtraFee,
          driverMaxExtraFee = Just $ maybe 0 (roundToIntegral . fst) maxExtraFee,
          driverMaxExtraFeeBreakup = snd <$> maxExtraFee,
          driverMaxExtraFeeWithCurrency = netPrice <$> maxExtraFee,
          driverDefaultStepFeeWithCurrency = netPrice <$> deprecatedDefaultStepFee,
          driverDefaultStepFeeWithCurrencyV2 = netPrice <$> defaultStepFee,
          driverDefaultStepFeeBreakup = snd <$> defaultStepFee,
          driverStepFeeWithCurrency = netPrice <$> stepFee,
          driverStepFeeBreakup = snd <$> stepFee,
          rideRequestPopupDelayDuration = delayDuration,
          specialLocationTag = searchRequest.specialLocationTag,
          disabilityTag = searchRequest.disabilityTag,
          keepHiddenForSeconds = keepHiddenForSeconds,
          goHomeRequestId = nearbyReq.goHomeRequestId,
          tripCategory = searchTry.tripCategory,
          isScheduled = searchTry.isScheduled,
          preferenceMatchScore =
            if fromMaybe False nearbyReq.hasApplicablePreferences
              then nearbyReq.preferenceMatchScore
              else Nothing,
          duration = searchRequest.estimatedDuration,
          pickupZone = nearbyReq.pickupZone,
          driverPickUpCharges = roundToIntegral . fst <$> pickUp,
          driverPickUpChargesWithCurrency = netPrice <$> pickUp,
          driverPickUpChargesBreakup = snd <$> pickUp,
          specialZoneExtraTip = roundToIntegral . fst <$> zoneExtraTip,
          specialZoneExtraTipBreakup = snd <$> zoneExtraTip,
          specialZoneExtraTipWithCurrency = netPrice <$> zoneExtraTip,
          vehicleServiceTier = nearbyReq.vehicleServiceTierName,
          airConditioned = nearbyReq.airConditioned,
          requestedVehicleVariant = DV.castServiceTierToVariant requestedVehicleServiceTier,
          tollCharges = fst <$> toll,
          tollChargesWithCurrency = flip PriceAPIEntity searchRequest.currency . fst <$> toll,
          tollChargesBreakup = snd <$> toll,
          parkingCharge = fst <$> parking,
          parkingChargeBreakup = snd <$> parking,
          tollNames = if isTollApplicable then searchRequest.tollNames else Nothing,
          useSilentFCMForForwardBatch = useSilentFCMForForwardBatch && nearbyReq.isForwardRequest,
          isOnRide = nearbyReq.isForwardRequest,
          isReferredRideReq = searchRequest.driverIdForSearch $> True,
          isFavourite = nearbyReq.isFavourite,
          roundTrip = searchRequest.roundTrip,
          returnTime = searchRequest.returnTime,
          cancellationCharges = roundToIntegral . fst <$> cancellation,
          cancellationChargesBreakup = snd <$> cancellation,
          middleStopCount = fromMaybe 0 nearbyReq.middleStopCount,
          parcelType = nearbyReq.parcelType,
          parcelQuantity = nearbyReq.parcelQuantity,
          conditionalCharges = nearbyReq.conditionalCharges,
          isSafetyPlus = fromMaybe False nearbyReq.isSafetyPlus,
          coinsRewardedOnGoldTierRide = nearbyReq.coinsRewardedOnGoldTierRide,
          safetyPlusCharges = Just (fst safetyPlus),
          safetyPlusChargesBreakup = Just (snd safetyPlus),
          commissionCharges = nearbyReq.commissionCharges,
          isPaymentOnline = deriveIsPaymentOnline searchTry.paymentInstrument,
          driverCancellationNotAllowed = nearbyReq.driverCancellationNotAllowed,
          customerRating = mbRiderDetails >>= (.customerRating),
          ..
        }
  where
    netPrice (amount, _) = PriceAPIEntity amount nearbyReq.currency
    minNonZero Nothing b = b
    minNonZero (Just 0) b = b
    minNonZero a b = min a b
    deriveIsPaymentOnline :: Maybe DMPM.PaymentInstrument -> Maybe Bool
    deriveIsPaymentOnline = fmap (\case DMPM.Cash -> False; _ -> True)

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
