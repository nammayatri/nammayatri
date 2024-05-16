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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverFee as Beam
import qualified Storage.Queries.Transformers.DriverFee

instance FromTType' Beam.DriverFee Domain.Types.DriverFee.DriverFee where
  fromTType' (Beam.DriverFeeT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.DriverFee.getMerchantOperatingCityId merchantOperatingCityId driverId id
    pure $
      Just
        Domain.Types.DriverFee.DriverFee
          { amountPaidByCoin = amountPaidByCoin,
            autopayPaymentStage = autopayPaymentStage,
            badDebtDeclarationDate = badDebtDeclarationDate,
            badDebtRecoveryDate = badDebtRecoveryDate,
            billNumber = billNumber,
            collectedAt = collectedAt,
            collectedBy = collectedBy,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            driverId = Kernel.Types.Id.Id driverId,
            endTime = endTime,
            feeType = feeType,
            feeWithoutDiscount = feeWithoutDiscount,
            govtCharges = Kernel.Types.Common.mkAmountWithDefault govtChargesAmount govtCharges,
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
            schedulerTryCount = schedulerTryCount,
            serviceName = fromMaybe Domain.Types.Plan.YATRI_SUBSCRIPTION serviceName,
            specialZoneAmount = specialZoneAmount,
            specialZoneRideCount = specialZoneRideCount,
            stageUpdatedAt = stageUpdatedAt,
            startTime = startTime,
            status = status,
            totalEarnings = Kernel.Types.Common.mkAmountWithDefault totalEarningsAmount totalEarnings,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber
          }

instance ToTType' Beam.DriverFee Domain.Types.DriverFee.DriverFee where
  toTType' (Domain.Types.DriverFee.DriverFee {..}) = do
    Beam.DriverFeeT
      { Beam.amountPaidByCoin = amountPaidByCoin,
        Beam.autopayPaymentStage = autopayPaymentStage,
        Beam.badDebtDeclarationDate = badDebtDeclarationDate,
        Beam.badDebtRecoveryDate = badDebtRecoveryDate,
        Beam.billNumber = billNumber,
        Beam.collectedAt = collectedAt,
        Beam.collectedBy = collectedBy,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.endTime = endTime,
        Beam.feeType = feeType,
        Beam.feeWithoutDiscount = feeWithoutDiscount,
        Beam.govtCharges = Kernel.Prelude.roundToIntegral govtCharges,
        Beam.govtChargesAmount = Kernel.Prelude.Just govtCharges,
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
        Beam.schedulerTryCount = schedulerTryCount,
        Beam.serviceName = Just serviceName,
        Beam.specialZoneAmount = specialZoneAmount,
        Beam.specialZoneRideCount = specialZoneRideCount,
        Beam.stageUpdatedAt = stageUpdatedAt,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.totalEarnings = Kernel.Prelude.roundToIntegral totalEarnings,
        Beam.totalEarningsAmount = Kernel.Prelude.Just totalEarnings,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber
      }
