{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverFee where

import qualified Domain.Types.DriverFee
import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverFee as Beam
import qualified Storage.Queries.Transformers.DriverFee

instance FromTType' Beam.DriverFee Domain.Types.DriverFee.DriverFee where
  fromTType' (Beam.DriverFeeT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.DriverFee.getMerchantOperatingCityId merchantOperatingCityId driverId id
    vehicleCategory' <- Storage.Queries.Transformers.DriverFee.getCategoryFromPlanOrSubscriptionConfig vehicleCategory planId planMode merchantOperatingCityId serviceName id driverId
    pure $
      Just
        Domain.Types.DriverFee.DriverFee
          { addedToFeeId = Kernel.Types.Id.Id <$> addedToFeeId,
            amountPaidByCoin = amountPaidByCoin,
            autopayPaymentStage = autopayPaymentStage,
            badDebtDeclarationDate = badDebtDeclarationDate,
            badDebtRecoveryDate = badDebtRecoveryDate,
            billNumber = billNumber,
            cancellationPenaltyAmount = cancellationPenaltyAmount,
            collectedAt = collectedAt,
            collectedAtVendorId = collectedAtVendorId,
            collectedBy = collectedBy,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            driverId = Kernel.Types.Id.Id driverId,
            endTime = endTime,
            feeType = feeType,
            feeWithoutDiscount = feeWithoutDiscount,
            govtCharges = Kernel.Types.Common.mkAmountWithDefault govtChargesAmount govtCharges,
            hasSibling = hasSibling,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            notificationRetryCount = notificationRetryCount,
            numRides = numRides,
            offerId = offerId,
            overlaySent = overlaySent,
            payBy = payBy,
            planId = Kernel.Types.Id.Id <$> planId,
            planMode = planMode,
            planOfferTitle = planOfferTitle,
            platformFee = Storage.Queries.Transformers.DriverFee.mkPlatformFee platformFee cgst sgst (Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency),
            refundEntityId = refundEntityId,
            refundedAmount = refundedAmount,
            refundedAt = refundedAt,
            refundedBy = refundedBy,
            schedulerTryCount = schedulerTryCount,
            serviceName = fromMaybe Domain.Types.Plan.YATRI_SUBSCRIPTION serviceName,
            siblingFeeId = Kernel.Types.Id.Id <$> siblingFeeId,
            specialZoneAmount = specialZoneAmount,
            specialZoneRideCount = specialZoneRideCount,
            splitOfDriverFeeId = Kernel.Types.Id.Id <$> splitOfDriverFeeId,
            stageUpdatedAt = stageUpdatedAt,
            startTime = startTime,
            status = status,
            totalEarnings = Kernel.Types.Common.mkAmountWithDefault totalEarningsAmount totalEarnings,
            updatedAt = updatedAt,
            validDays = validDays,
            vehicleCategory = vehicleCategory',
            vehicleNumber = vehicleNumber
          }

instance ToTType' Beam.DriverFee Domain.Types.DriverFee.DriverFee where
  toTType' (Domain.Types.DriverFee.DriverFee {..}) = do
    Beam.DriverFeeT
      { Beam.addedToFeeId = Kernel.Types.Id.getId <$> addedToFeeId,
        Beam.amountPaidByCoin = amountPaidByCoin,
        Beam.autopayPaymentStage = autopayPaymentStage,
        Beam.badDebtDeclarationDate = badDebtDeclarationDate,
        Beam.badDebtRecoveryDate = badDebtRecoveryDate,
        Beam.billNumber = billNumber,
        Beam.cancellationPenaltyAmount = cancellationPenaltyAmount,
        Beam.collectedAt = collectedAt,
        Beam.collectedAtVendorId = collectedAtVendorId,
        Beam.collectedBy = collectedBy,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.endTime = endTime,
        Beam.feeType = feeType,
        Beam.feeWithoutDiscount = feeWithoutDiscount,
        Beam.govtCharges = Kernel.Prelude.roundToIntegral govtCharges,
        Beam.govtChargesAmount = Kernel.Prelude.Just govtCharges,
        Beam.hasSibling = hasSibling,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just (Kernel.Types.Id.getId merchantOperatingCityId),
        Beam.notificationRetryCount = notificationRetryCount,
        Beam.numRides = numRides,
        Beam.offerId = offerId,
        Beam.overlaySent = overlaySent,
        Beam.payBy = payBy,
        Beam.planId = Kernel.Types.Id.getId <$> planId,
        Beam.planMode = planMode,
        Beam.planOfferTitle = planOfferTitle,
        Beam.cgst = (.cgst) platformFee,
        Beam.platformFee = (.fee) platformFee,
        Beam.sgst = (.sgst) platformFee,
        Beam.refundEntityId = refundEntityId,
        Beam.refundedAmount = refundedAmount,
        Beam.refundedAt = refundedAt,
        Beam.refundedBy = refundedBy,
        Beam.schedulerTryCount = schedulerTryCount,
        Beam.serviceName = Just serviceName,
        Beam.siblingFeeId = Kernel.Types.Id.getId <$> siblingFeeId,
        Beam.specialZoneAmount = specialZoneAmount,
        Beam.specialZoneRideCount = specialZoneRideCount,
        Beam.splitOfDriverFeeId = Kernel.Types.Id.getId <$> splitOfDriverFeeId,
        Beam.stageUpdatedAt = stageUpdatedAt,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.totalEarnings = Kernel.Prelude.roundToIntegral totalEarnings,
        Beam.totalEarningsAmount = Kernel.Prelude.Just totalEarnings,
        Beam.updatedAt = updatedAt,
        Beam.validDays = validDays,
        Beam.vehicleCategory = Kernel.Prelude.Just vehicleCategory,
        Beam.vehicleNumber = vehicleNumber
      }
