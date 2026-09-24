module SharedLogic.FarePolicy.Conversions where

import qualified "this" API.Types.ProviderPlatform.Management.Merchant as DPM
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.CancellationFarePolicy as DTC
import qualified Domain.Types.ConditionalCharges as DTAC
import Domain.Types.FarePolicy
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Types.SpecialLocation as SL

type FullDriverExtraFeeBounds = (Id FarePolicy, DriverExtraFeeBounds)

type FullFarePolicyProgressiveDetails = (Id FarePolicy, FPProgressiveDetails)

type FullFarePolicyRentalDetails = (Id FarePolicy, FPRentalDetails)

type FullFarePolicyInterCityDetails = (Id FarePolicy, FPInterCityDetails)

data FullFarePolicyD (s :: DTC.UsageSafety) = FullFarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id Merchant,
    vehicleServiceTier :: DVST.ServiceTierType,
    tripCategory :: DTC.TripCategory,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe HighPrecMoney,
    perStopCharge :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    perLuggageCharge :: Maybe HighPrecMoney,
    returnFee :: Maybe ReturnFee,
    boothCharges :: Maybe BoothCharge,
    schedulingCharge :: Maybe SchedulingCharge,
    currency :: Currency,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    tipOptions :: Maybe [Int],
    distanceUnit :: DistanceUnit,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    driverAllowance :: Maybe HighPrecMoney,
    airportConvenienceFee :: Maybe HighPrecMoney,
    businessDiscountPercentage :: Maybe Double,
    personalDiscountPercentage :: Maybe Double,
    priorityCharges :: Maybe HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Maybe Seconds,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    rideExtraTimeChargeGracePeriod :: Maybe Seconds,
    congestionChargeMultiplier :: Maybe CongestionChargeMultiplier,
    congestionChargePerMin :: Maybe Double,
    dpVersion :: Maybe Text,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    additionalCongestionCharge :: HighPrecMoney,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    shadowSurgeMultiplier :: Maybe Centesimal,
    shadowSurgeVersion :: Maybe Int,
    perDistanceUnitInsuranceCharge :: Maybe HighPrecMoney,
    cardCharge :: Maybe CardCharge,
    vatChargeConfig :: Maybe FareChargeConfig,
    commissionChargeConfig :: Maybe FareChargeConfig,
    cancellationCommissionChargeConfig :: Maybe FareChargeConfig,
    tollTaxChargeConfig :: Maybe FareChargeConfig,
    farePolicyDetails :: FarePolicyDetailsD 'DTC.Safe,
    description :: Maybe Text,
    cancellationFarePolicy :: Maybe DTC.CancellationFarePolicy,
    platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    platformFeeChargesBy :: PlatformFeeMethods,
    disableRecompute :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARCity :: Maybe Double,
    conditionalCharges :: [DTAC.ConditionalCharges],
    congestionChargeData :: Maybe CongestionChargeData,
    driverCancellationNotAllowed :: Maybe Bool,
    mbArea :: Maybe SL.Area,
    fareSettlementType :: Maybe SL.FareSettlementType
  }
  deriving (Generic, Show)

type FullFarePolicy = FullFarePolicyD 'DTC.Safe

instance FromJSON (FullFarePolicyD 'DTC.Unsafe)

instance ToJSON (FullFarePolicyD 'DTC.Unsafe)

instance FromJSON FullFarePolicy

instance ToJSON FullFarePolicy

farePolicyToFullFarePolicy :: Id Merchant -> DVST.ServiceTierType -> DTC.TripCategory -> Maybe DTC.CancellationFarePolicy -> CongestionChargeDetails -> Maybe CongestionChargeData -> FarePolicy -> Maybe Bool -> FullFarePolicy
farePolicyToFullFarePolicy merchantId' vehicleServiceTier tripCategory cancellationFarePolicy CongestionChargeDetails {..} congestionChargeData FarePolicy {..} disableRecompute =
  FullFarePolicy
    { merchantId = merchantId',
      mbArea = Nothing,
      fareSettlementType = Nothing,
      driverExtraFeeBounds = driverExtraFeeBounds,
      ..
    }

fullFarePolicyToFarePolicy :: FullFarePolicy -> FarePolicy
fullFarePolicyToFarePolicy ffp@FullFarePolicy {..} =
  let cancellationFarePolicyId = (.id) <$> ffp.cancellationFarePolicy
   in FarePolicy
        { merchantId = Just merchantId,
          driverExtraFeeBounds = driverExtraFeeBounds,
          govtCharges = Nothing,
          ..
        }

getFarePolicyType :: FarePolicy -> FarePolicyType
getFarePolicyType farePolicy = case farePolicy.farePolicyDetails of
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
  AmbulanceDetails _ -> Ambulance
