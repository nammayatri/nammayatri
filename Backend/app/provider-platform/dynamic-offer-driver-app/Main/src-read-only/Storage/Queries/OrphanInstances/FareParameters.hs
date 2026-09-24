{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareParameters where

import qualified Data.Aeson
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareParameters as Beam
import qualified Storage.Queries.Transformers.FareParameters

instance FromTType' Beam.FareParameters Domain.Types.FareParameters.FareParameters where
  fromTType' (Beam.FareParametersT {..}) = do
    mFareParametersDetails <- Storage.Queries.Transformers.FareParameters.fetchFareParametersDetails fareParametersType id
    now <- Kernel.Types.Common.getCurrentTime
    pure $
      Just
        Domain.Types.FareParameters.FareParameters
          { airportConvenienceFee = airportConvenienceFee,
            baseFare = Kernel.Types.Common.mkAmountWithDefault baseFareAmount baseFare,
            boothCharge = boothCharge,
            businessDiscount = businessDiscount,
            cancellationFeeTaxExclusive = cancellationFeeTaxExclusive,
            cancellationTax = cancellationTax,
            cardCharge = Storage.Queries.Transformers.FareParameters.mkCardCharge cardChargeOnFare fixedCardCharge,
            cgst = cgst,
            commission = Kernel.Prelude.Nothing,
            conditionalCharges = Storage.Queries.Transformers.FareParameters.decodeConditionalCharges conditionalCharges,
            congestionCharge = Kernel.Types.Common.mkAmountWithDefault congestionChargeAmount <$> congestionCharge,
            congestionChargeViaDp = congestionChargeViaDp,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            customerCancellationDues = customerCancellationDues,
            customerExtraFee = Kernel.Types.Common.mkAmountWithDefault customerExtraFeeAmount <$> customerExtraFee,
            customerGateFeeItems = Storage.Queries.Transformers.FareParameters.decodeCustomerGateFeeItems customerGateFeeItems,
            discountApplicableRideFareTax = discountApplicableRideFareTax,
            discountApplicableRideFareTaxExclusive = discountApplicableRideFareTaxExclusive,
            driverAllowance = driverAllowance,
            driverCancellationNotAllowed = driverCancellationNotAllowed,
            driverSelectedFare = Kernel.Types.Common.mkAmountWithDefault driverSelectedFareAmount <$> driverSelectedFare,
            fareParametersDetails = Storage.Queries.Transformers.FareParameters.fromMaybeFareParametersDetails mFareParametersDetails,
            fareSettlementType = fareSettlementType,
            govtCharges = Kernel.Types.Common.mkAmountWithDefault govtChargesAmount <$> govtCharges,
            id = Kernel.Types.Id.Id id,
            insuranceCharge = insuranceCharge,
            isVatTaxType = isVatTaxType,
            luggageCharge = luggageCharge,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            negativeFareAdjustment = Kernel.Types.Common.mkAmountWithDefault negativeFareAdjustmentAmount <$> negativeFareAdjustment,
            negotiatedFareDelta = negotiatedFareDelta,
            nightShiftCharge = Kernel.Types.Common.mkAmountWithDefault nightShiftChargeAmount <$> nightShiftCharge,
            nightShiftRateIfApplies = nightShiftRateIfApplies,
            nonDiscountApplicableRideFareTax = nonDiscountApplicableRideFareTax,
            nonDiscountApplicableRideFareTaxExclusive = nonDiscountApplicableRideFareTaxExclusive,
            parkingCharge = parkingCharge,
            parkingChargeTax = parkingChargeTax,
            parkingChargeTaxExclusive = parkingChargeTaxExclusive,
            paymentProcessingFee = paymentProcessingFee,
            paymentProcessingFeeVat = paymentProcessingFeeVat,
            personalDiscount = personalDiscount,
            petCharges = petCharges,
            platformFee = platformFee,
            platformFeeChargesBy = Kernel.Prelude.fromMaybe Domain.Types.FarePolicy.Subscription platformFeeChargesBy,
            priorityCharges = priorityCharges,
            returnFeeCharge = returnFeeCharge,
            rideExtraTimeFare = Kernel.Types.Common.mkAmountWithDefault rideExtraTimeFareAmount <$> rideExtraTimeFare,
            schedulingCharge = schedulingCharge,
            serviceCharge = Kernel.Types.Common.mkAmountWithDefault serviceChargeAmount <$> serviceCharge,
            sgst = sgst,
            shouldApplyBusinessDiscount = Kernel.Prelude.fromMaybe Kernel.Prelude.False shouldApplyBusinessDiscount,
            shouldApplyPersonalDiscount = Kernel.Prelude.fromMaybe Kernel.Prelude.False shouldApplyPersonalDiscount,
            stopCharges = stopCharges,
            tdsAmount = tdsAmount,
            tdsProcessedAt = tdsProcessedAt,
            tdsRate = tdsRate,
            tollCharges = tollCharges,
            tollFareTax = tollVat,
            tollFareTaxExclusive = tollFareTaxExclusive,
            updatedAt = Kernel.Prelude.fromMaybe now updatedAt,
            waitingCharge = Kernel.Types.Common.mkAmountWithDefault waitingChargeAmount <$> waitingCharge
          }

instance ToTType' Beam.FareParameters Domain.Types.FareParameters.FareParameters where
  toTType' (Domain.Types.FareParameters.FareParameters {..}) = do
    Beam.FareParametersT
      { Beam.airportConvenienceFee = airportConvenienceFee,
        Beam.baseFare = Kernel.Prelude.roundToIntegral baseFare,
        Beam.baseFareAmount = Kernel.Prelude.Just baseFare,
        Beam.boothCharge = boothCharge,
        Beam.businessDiscount = businessDiscount,
        Beam.cancellationFeeTaxExclusive = cancellationFeeTaxExclusive,
        Beam.cancellationTax = cancellationTax,
        Beam.cardChargeOnFare = cardCharge Kernel.Prelude.>>= (.onFare),
        Beam.fixedCardCharge = cardCharge Kernel.Prelude.>>= (.fixed),
        Beam.cgst = cgst,
        Beam.commission = Kernel.Prelude.Nothing,
        Beam.conditionalCharges = Kernel.Prelude.Just $ Data.Aeson.toJSON conditionalCharges,
        Beam.congestionCharge = Kernel.Prelude.roundToIntegral <$> congestionCharge,
        Beam.congestionChargeAmount = congestionCharge,
        Beam.congestionChargeViaDp = congestionChargeViaDp,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.customerCancellationDues = customerCancellationDues,
        Beam.customerExtraFee = Kernel.Prelude.roundToIntegral <$> customerExtraFee,
        Beam.customerExtraFeeAmount = customerExtraFee,
        Beam.customerGateFeeItems = Kernel.Prelude.Just $ Data.Aeson.toJSON customerGateFeeItems,
        Beam.discountApplicableRideFareTax = discountApplicableRideFareTax,
        Beam.discountApplicableRideFareTaxExclusive = discountApplicableRideFareTaxExclusive,
        Beam.driverAllowance = driverAllowance,
        Beam.driverCancellationNotAllowed = driverCancellationNotAllowed,
        Beam.driverSelectedFare = Kernel.Prelude.roundToIntegral <$> driverSelectedFare,
        Beam.driverSelectedFareAmount = driverSelectedFare,
        Beam.fareParametersType = Storage.Queries.Transformers.FareParameters.mkFareParametersType fareParametersDetails,
        Beam.fareSettlementType = fareSettlementType,
        Beam.govtCharges = Kernel.Prelude.roundToIntegral <$> govtCharges,
        Beam.govtChargesAmount = govtCharges,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuranceCharge = insuranceCharge,
        Beam.isVatTaxType = isVatTaxType,
        Beam.luggageCharge = luggageCharge,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.negativeFareAdjustment = Kernel.Prelude.roundToIntegral <$> negativeFareAdjustment,
        Beam.negativeFareAdjustmentAmount = negativeFareAdjustment,
        Beam.negotiatedFareDelta = negotiatedFareDelta,
        Beam.nightShiftCharge = Kernel.Prelude.roundToIntegral <$> nightShiftCharge,
        Beam.nightShiftChargeAmount = nightShiftCharge,
        Beam.nightShiftRateIfApplies = nightShiftRateIfApplies,
        Beam.nonDiscountApplicableRideFareTax = nonDiscountApplicableRideFareTax,
        Beam.nonDiscountApplicableRideFareTaxExclusive = nonDiscountApplicableRideFareTaxExclusive,
        Beam.parkingCharge = parkingCharge,
        Beam.parkingChargeTax = parkingChargeTax,
        Beam.parkingChargeTaxExclusive = parkingChargeTaxExclusive,
        Beam.paymentProcessingFee = paymentProcessingFee,
        Beam.paymentProcessingFeeVat = paymentProcessingFeeVat,
        Beam.personalDiscount = personalDiscount,
        Beam.petCharges = petCharges,
        Beam.platformFee = platformFee,
        Beam.platformFeeChargesBy = Kernel.Prelude.Just platformFeeChargesBy,
        Beam.priorityCharges = priorityCharges,
        Beam.returnFeeCharge = returnFeeCharge,
        Beam.rideExtraTimeFare = Kernel.Prelude.roundToIntegral <$> rideExtraTimeFare,
        Beam.rideExtraTimeFareAmount = rideExtraTimeFare,
        Beam.schedulingCharge = schedulingCharge,
        Beam.serviceCharge = Kernel.Prelude.roundToIntegral <$> serviceCharge,
        Beam.serviceChargeAmount = serviceCharge,
        Beam.sgst = sgst,
        Beam.shouldApplyBusinessDiscount = Kernel.Prelude.Just shouldApplyBusinessDiscount,
        Beam.shouldApplyPersonalDiscount = Kernel.Prelude.Just shouldApplyPersonalDiscount,
        Beam.stopCharges = stopCharges,
        Beam.tdsAmount = tdsAmount,
        Beam.tdsProcessedAt = tdsProcessedAt,
        Beam.tdsRate = tdsRate,
        Beam.tollCharges = tollCharges,
        Beam.tollVat = tollFareTax,
        Beam.tollFareTaxExclusive = tollFareTaxExclusive,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.waitingCharge = Kernel.Prelude.roundToIntegral <$> waitingCharge,
        Beam.waitingChargeAmount = waitingCharge
      }
