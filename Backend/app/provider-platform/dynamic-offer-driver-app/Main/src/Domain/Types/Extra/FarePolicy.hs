{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.FarePolicy
  ( module Domain.Types.Extra.FarePolicy,
    module Reexport,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.Merchant as DPM
import Data.Aeson.Types
import qualified Data.List as DL
import Data.List.NonEmpty
import Data.Text as Text
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.CancellationFarePolicy as DTC
import qualified Domain.Types.ConditionalCharges as DTAC
import Domain.Types.FarePolicy.DriverExtraFeeBounds as Reexport
import Domain.Types.FarePolicy.FarePolicyAmbulanceDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyInterCityDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyRentalDetails as Reexport
import Domain.Types.FarePolicy.FarePolicySlabsDetails as Reexport
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.GenericPretty
import qualified Lib.Types.SpecialLocation as SL
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum, mkBeamInstancesForJSON)

data ReturnFee
  = ReturnFeeFixed HighPrecMoney
  | ReturnFeePercentage Double
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''ReturnFee)

data BoothCharge
  = BoothChargeFixed HighPrecMoney
  | BoothChargePercentage Double
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''BoothCharge)

data SchedulingCharge
  = ProgressiveSchedulingCharge Double
  | ConstantSchedulingCharge HighPrecMoney
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''SchedulingCharge)

data AllowedTripDistanceBounds = AllowedTripDistanceBounds
  { maxAllowedTripDistance :: Meters,
    minAllowedTripDistance :: Meters,
    distanceUnit :: DistanceUnit
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

mkAllowedTripDistanceBounds :: DistanceUnit -> DPM.AllowedTripDistanceBoundsAPIEntity -> AllowedTripDistanceBounds
mkAllowedTripDistanceBounds distanceUnit DPM.AllowedTripDistanceBoundsAPIEntity {..} =
  AllowedTripDistanceBounds
    { maxAllowedTripDistance = maybe maxAllowedTripDistance distanceToMeters maxAllowedTripDistanceWithUnit,
      minAllowedTripDistance = maybe minAllowedTripDistance distanceToMeters minAllowedTripDistanceWithUnit,
      distanceUnit
    }

data FarePolicyDetailsD (s :: DTC.UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s) | RentalDetails (FPRentalDetailsD s) | InterCityDetails (FPInterCityDetailsD s) | AmbulanceDetails (FPAmbulanceDetailsD s)
  deriving (Generic, Show, ToSchema)

type FarePolicyDetails = FarePolicyDetailsD 'DTC.Safe

instance FromJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance ToJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance FromJSON (FarePolicyDetailsD 'DTC.Safe)

instance ToJSON (FarePolicyDetailsD 'DTC.Safe)

data CardCharge = CardCharge
  { perDistanceUnitMultiplier :: Maybe Double,
    fixed :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareChargeComponent
  = RideFare
  | WaitingCharge
  | ServiceChargeComponent
  | TollChargesComponent
  | CongestionChargeComponent
  | ParkingChargeComponent
  | PetChargeComponent
  | PriorityChargeComponent
  | NightShiftChargeComponent
  | InsuranceChargeComponent
  | StopChargeComponent
  | LuggageChargeComponent
  | PlatformFeeComponent
  | CustomerCancellationChargeComponent
  | CustomerExtraFeeComponent
  | DeadKmFareComponent
  | ExtraKmFareComponent
  | RideDurationFareComponent
  | TimeBasedFareComponent
  | DistBasedFareComponent
  | TimeFareComponent
  | DistanceFareComponent
  | PickupChargeComponent
  | ExtraDistanceFareComponent
  | ExtraTimeFareComponent
  | StateEntryPermitChargesComponent
  | AddOnChargeComponent
  | AmbulanceDistBasedFareComponent
  | RideVatComponent
  | TollVatComponent
  | DriverAllowanceComponent
  | AirportConvenienceFeeComponent
  | ReturnFeeChargeComponent
  | BoothChargeComponent
  | RideExtraTimeFareComponent
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareChargeConfig = FareChargeConfig
  { value :: Text,
    appliesOn :: [FareChargeComponent]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CapStrategy
  = PercentCap PercentCapCfg
  | FixedCap FixedCapCfg
  | Frozen
  | Derived
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PercentCapCfg = PercentCapCfg
  { percent :: Double,
    minCapAmount :: Maybe HighPrecMoney,
    maxCapAmount :: Maybe HighPrecMoney
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype FixedCapCfg = FixedCapCfg
  { amount :: HighPrecMoney
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareRecomputeCap = FareRecomputeCap
  { strategy :: CapStrategy,
    appliesOn :: [FareChargeComponent]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype FareRecomputeCapConfig = FareRecomputeCapConfig
  { caps :: [FareRecomputeCap]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

lookupCapStrategy :: FareRecomputeCapConfig -> FareChargeComponent -> Maybe CapStrategy
lookupCapStrategy capConfig component =
  strategy <$> KP.find (\cap -> component `KP.elem` cap.appliesOn) capConfig.caps

validateFareRecomputeCapConfig :: FareRecomputeCapConfig -> Either Text ()
validateFareRecomputeCapConfig capConfig = do
  KP.mapM_ validateCap capConfig.caps
  validateNoOverlap (KP.concatMap (.appliesOn) capConfig.caps)
  where
    validateCap cap = case cap.strategy of
      PercentCap cfg -> do
        KP.when (cfg.percent < 0) $ Left $ "Fare recompute cap: percent must be >= 0, got " <> KP.show cfg.percent
        KP.when (cfg.percent > 100) $ Left $ "Fare recompute cap: percent must be <= 100, got " <> KP.show cfg.percent
        validateNonNegative "minCapAmount" cfg.minCapAmount
        validateNonNegative "maxCapAmount" cfg.maxCapAmount
        case (cfg.minCapAmount, cfg.maxCapAmount) of
          (Just minAmt, Just maxAmt) ->
            KP.when (minAmt > maxAmt) $
              Left $ "Fare recompute cap: minCapAmount (" <> KP.show minAmt <> ") must not exceed maxCapAmount (" <> KP.show maxAmt <> ")"
          _ -> Right ()
      FixedCap cfg -> validateNonNegative "amount" (Just cfg.amount)
      Frozen -> Right ()
      Derived -> Right ()
    validateNonNegative label = KP.maybe (Right ()) $ \amt ->
      KP.when (amt < 0) $ Left $ "Fare recompute cap: " <> label <> " must be >= 0, got " <> KP.show amt
    validateNoOverlap allComponents =
      let duplicates = DL.nub (KP.filter (\c -> KP.length (KP.filter (== c) allComponents) > 1) allComponents)
       in KP.unless (KP.null duplicates) $
            Left $ "Fare recompute cap: component(s) appear in more than one cap rule (ambiguous): " <> KP.show duplicates

capAllowance :: CapStrategy -> HighPrecMoney -> HighPrecMoney
capAllowance capStrategy estimate = case capStrategy of
  Frozen -> 0
  Derived -> 0
  FixedCap cfg -> cfg.amount
  PercentCap cfg ->
    let rawAllowance = estimate * realToFrac cfg.percent / 100
        flooredAllowance = maybe rawAllowance (`max` rawAllowance) cfg.minCapAmount
     in maybe flooredAllowance (`min` flooredAllowance) cfg.maxCapAmount

capByStrategy :: Maybe CapStrategy -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney
capByStrategy Nothing estimate recomputedValue = min recomputedValue estimate
capByStrategy (Just capStrategy) estimate recomputedValue =
  min recomputedValue (estimate + capAllowance capStrategy estimate)

data CongestionChargeMultiplier
  = BaseFareAndExtraDistanceFare Centesimal
  | ExtraDistanceFare Centesimal
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PlatformFeeMethods = Subscription | FixedAmount | None | SlabBased | NoCharge
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlatformFeeMethods

data FarePolicyType = Progressive | Slabs | Rental | InterCity | Ambulance
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FarePolicyType)
$(mkBeamInstancesForJSON ''CongestionChargeMultiplier)
$(mkBeamInstancesForJSON ''FareRecomputeCapConfig)
$(mkBeamInstancesForEnum ''PlatformFeeMethods)

defaultNegotiationTolerancePct :: Double
defaultNegotiationTolerancePct = 10

maxNegotiationTolerancePct :: Double
maxNegotiationTolerancePct = 100

effectiveNegotiationTolerancePct :: Maybe Double -> Double
effectiveNegotiationTolerancePct = max 0 . min maxNegotiationTolerancePct . fromMaybe defaultNegotiationTolerancePct

data CongestionChargeDetails = CongestionChargeDetails
  { dpVersion :: Maybe Text,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    congestionChargePerMin :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARCity :: Maybe Double,
    shadowSurgeMultiplier :: Maybe Centesimal,
    shadowSurgeVersion :: Maybe Int,
    fareAdjustmentId :: Maybe Text,
    fareAdjustmentArm :: Maybe Text
  }
  deriving (Generic, Show)

data CongestionChargeData = CongestionChargeData
  { mbActualQARFromLocGeohashDistancePast :: Maybe Double,
    mbActualQARFromLocGeohashPast :: Maybe Double,
    mbActualQARCityPast :: Maybe Double,
    mbCongestionFromLocGeohashDistance :: Maybe Double,
    mbCongestionFromLocGeohashDistancePast :: Maybe Double,
    mbCongestionFromLocGeohash :: Maybe Double,
    mbCongestionFromLocGeohashPast :: Maybe Double,
    mbCongestionCity :: Maybe Double,
    mbCongestionCityPast :: Maybe Double,
    mbActualQARFromLocGeohashDistance :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

mkCongestionChargeMultiplier :: DPM.CongestionChargeMultiplierAPIEntity -> CongestionChargeMultiplier
mkCongestionChargeMultiplier (DPM.BaseFareAndExtraDistanceFare charge) = BaseFareAndExtraDistanceFare charge
mkCongestionChargeMultiplier (DPM.ExtraDistanceFare charge) = ExtraDistanceFare charge

congestionChargeMultiplierToCentesimal :: CongestionChargeMultiplier -> Centesimal
congestionChargeMultiplierToCentesimal (BaseFareAndExtraDistanceFare charge) = charge
congestionChargeMultiplierToCentesimal (ExtraDistanceFare charge) = charge
