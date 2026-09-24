{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicy where

import qualified Data.List.NonEmpty
import qualified Domain.Types.Extra.FarePolicy
import qualified Domain.Types.FarePolicy
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicy as Beam
import qualified Storage.Queries.Transformers.FarePolicy

instance FromTType' Beam.FarePolicy Domain.Types.FarePolicy.FarePolicy where
  fromTType' (Beam.FarePolicyT {..}) = do
    mFarePolicyDetails <- Storage.Queries.Transformers.FarePolicy.fetchFarePolicyDetails farePolicyType id
    driverExtraFeeBoundsList <- Storage.Queries.Transformers.FarePolicy.fetchDriverExtraFeeBounds id
    conditionalChargesList <- Storage.Queries.Transformers.FarePolicy.fetchConditionalCharges id
    pure $
      Just
        Domain.Types.FarePolicy.FarePolicy
          { additionalCongestionCharge = (0),
            airportConvenienceFee = airportConvenienceFee,
            allowedTripDistanceBounds = Storage.Queries.Transformers.FarePolicy.mkAllowedTripDistanceBoundsFromBeam minAllowedTripDistance maxAllowedTripDistance distanceUnit,
            boothCharges = boothCharges,
            businessDiscountPercentage = businessDiscountPercentage,
            cancellationCommissionChargeConfig = Storage.Queries.Transformers.FarePolicy.decodeChargeConfig cancellationCommissionChargeConfig,
            cancellationFarePolicyId = Kernel.Types.Id.Id <$> cancellationFarePolicyId,
            cardCharge = Storage.Queries.Transformers.FarePolicy.mkCardCharge cardChargePerDistanceUnitMultiplier fixedCardCharge,
            cgst = cgst,
            commissionChargeConfig = Storage.Queries.Transformers.FarePolicy.decodeChargeConfig commissionChargeConfig,
            conditionalCharges = conditionalChargesList,
            congestionChargeMultiplier = congestionCharge,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            description = description,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverAllowance = driverAllowance,
            driverCancellationNotAllowed = driverCancellationNotAllowed,
            driverExtraFeeBounds = Data.List.NonEmpty.nonEmpty driverExtraFeeBoundsList,
            farePolicyDetails = Storage.Queries.Transformers.FarePolicy.fromMaybeFarePolicyDetails mFarePolicyDetails,
            govtCharges = Kernel.Prelude.Nothing,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            nightShiftBounds = Storage.Queries.Transformers.FarePolicy.mkNightShiftBounds nightShiftStart nightShiftEnd,
            parkingCharge = parkingCharge,
            perDistanceUnitInsuranceCharge = perDistanceUnitInsuranceCharge,
            perLuggageCharge = perLuggageCharge,
            perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
            perStopCharge = perStopCharge,
            personalDiscountPercentage = personalDiscountPercentage,
            petCharges = petCharges,
            pickupBufferInSecsForNightShiftCal = pickupBufferInSecsForNightShiftCal,
            platformFee = platformFee,
            platformFeeChargesBy = Kernel.Prelude.fromMaybe Domain.Types.Extra.FarePolicy.Subscription platformFeeChargesBy,
            priorityCharges = priorityCharges,
            returnFee = returnFee,
            rideExtraTimeChargeGracePeriod = rideExtraTimeChargeGracePeriod,
            schedulingCharge = schedulingCharge,
            serviceCharge = Kernel.Types.Common.mkAmountWithDefault serviceChargeAmount <$> serviceCharge,
            sgst = sgst,
            tipOptions = tipOptions,
            tollCharges = tollCharges,
            tollTaxChargeConfig = Storage.Queries.Transformers.FarePolicy.decodeChargeConfig tollTaxChargeConfig,
            updatedAt = updatedAt,
            vatChargeConfig = Storage.Queries.Transformers.FarePolicy.decodeChargeConfig vatChargeConfig
          }

instance ToTType' Beam.FarePolicy Domain.Types.FarePolicy.FarePolicy where
  toTType' (Domain.Types.FarePolicy.FarePolicy {..}) = do
    Beam.FarePolicyT
      { Beam.airportConvenienceFee = airportConvenienceFee,
        Beam.maxAllowedTripDistance = (.maxAllowedTripDistance) <$> allowedTripDistanceBounds,
        Beam.minAllowedTripDistance = (.minAllowedTripDistance) <$> allowedTripDistanceBounds,
        Beam.boothCharges = boothCharges,
        Beam.businessDiscountPercentage = businessDiscountPercentage,
        Beam.cancellationCommissionChargeConfig = Storage.Queries.Transformers.FarePolicy.encodeChargeConfig <$> cancellationCommissionChargeConfig,
        Beam.cancellationFarePolicyId = Kernel.Types.Id.getId <$> cancellationFarePolicyId,
        Beam.cardChargePerDistanceUnitMultiplier = (cardCharge Kernel.Prelude.>>= (.perDistanceUnitMultiplier)),
        Beam.fixedCardCharge = (cardCharge Kernel.Prelude.>>= (.fixed)),
        Beam.cgst = cgst,
        Beam.commissionChargeConfig = Storage.Queries.Transformers.FarePolicy.encodeChargeConfig <$> commissionChargeConfig,
        Beam.congestionCharge = congestionChargeMultiplier,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.description = description,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverAllowance = driverAllowance,
        Beam.driverCancellationNotAllowed = driverCancellationNotAllowed,
        Beam.farePolicyType = Storage.Queries.Transformers.FarePolicy.mkFarePolicyType farePolicyDetails,
        Beam.govtCharges = Kernel.Prelude.Nothing,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.nightShiftEnd = (.nightShiftEnd) <$> nightShiftBounds,
        Beam.nightShiftStart = (.nightShiftStart) <$> nightShiftBounds,
        Beam.parkingCharge = parkingCharge,
        Beam.perDistanceUnitInsuranceCharge = perDistanceUnitInsuranceCharge,
        Beam.perLuggageCharge = perLuggageCharge,
        Beam.perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
        Beam.perStopCharge = perStopCharge,
        Beam.personalDiscountPercentage = personalDiscountPercentage,
        Beam.petCharges = petCharges,
        Beam.pickupBufferInSecsForNightShiftCal = pickupBufferInSecsForNightShiftCal,
        Beam.platformFee = platformFee,
        Beam.platformFeeChargesBy = Kernel.Prelude.Just platformFeeChargesBy,
        Beam.priorityCharges = priorityCharges,
        Beam.returnFee = returnFee,
        Beam.rideExtraTimeChargeGracePeriod = rideExtraTimeChargeGracePeriod,
        Beam.schedulingCharge = schedulingCharge,
        Beam.serviceCharge = Kernel.Prelude.roundToIntegral <$> serviceCharge,
        Beam.serviceChargeAmount = serviceCharge,
        Beam.sgst = sgst,
        Beam.tipOptions = tipOptions,
        Beam.tollCharges = tollCharges,
        Beam.tollTaxChargeConfig = Storage.Queries.Transformers.FarePolicy.encodeChargeConfig <$> tollTaxChargeConfig,
        Beam.updatedAt = updatedAt,
        Beam.vatChargeConfig = Storage.Queries.Transformers.FarePolicy.encodeChargeConfig <$> vatChargeConfig
      }
