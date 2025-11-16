{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchTry where

import qualified Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified SharedLogic.Type
import qualified Storage.Beam.SearchTry as Beam
import qualified Storage.Queries.Transformers.SearchRequestForDriver
import Storage.Queries.Transformers.SearchTry

instance FromTType' Beam.SearchTry Domain.Types.SearchTry.SearchTry where
  fromTType' (Beam.SearchTryT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.SearchRequestForDriver.getMerchantOpCId merchantOperatingCityId merchantId requestId
    pure $
      Just
        Domain.Types.SearchTry.SearchTry
          { baseFare = Kernel.Types.Common.mkAmountWithDefault baseFareAmount baseFare,
            billingCategory = fromMaybe SharedLogic.Type.PERSONAL billingCategory,
            createdAt = createdAt,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            customerExtraFee = Kernel.Types.Common.mkAmountWithDefault customerExtraFeeAmount <$> customerExtraFee,
            estimateId = estimateId,
            estimateIds = fromMaybe [estimateId] estimateIds,
            id = Kernel.Types.Id.Id id,
            isAdvancedBookingEnabled = fromMaybe False isAdvancedBookingEnabled,
            isScheduled = fromMaybe Kernel.Prelude.False isScheduled,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            messageId = messageId,
            petCharges = Kernel.Types.Common.mkAmountWithDefault petChargesAmount <$> petCharges,
            preferSafetyPlus = fromMaybe False preferSafetyPlus,
            requestId = Kernel.Types.Id.Id requestId,
            searchRepeatCounter = searchRepeatCounter,
            searchRepeatType = searchRepeatType,
            serviceTierArray = fromMaybe (catMaybes [vehicleServiceTierName]) serviceTierArray,
            startTime = startTime,
            status = status,
            tripCategory = getTripCategory tripCategory,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleCategory = vehicleCategory,
            vehicleServiceTier = vehicleVariant,
            vehicleServiceTierName = fromMaybe (Kernel.Prelude.show vehicleVariant) vehicleServiceTierName
          }

instance ToTType' Beam.SearchTry Domain.Types.SearchTry.SearchTry where
  toTType' (Domain.Types.SearchTry.SearchTry {..}) = do
    Beam.SearchTryT
      { Beam.baseFare = Kernel.Prelude.roundToIntegral baseFare,
        Beam.baseFareAmount = Kernel.Prelude.Just baseFare,
        Beam.billingCategory = Kernel.Prelude.Just billingCategory,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.customerExtraFee = Kernel.Prelude.roundToIntegral <$> customerExtraFee,
        Beam.customerExtraFeeAmount = customerExtraFee,
        Beam.estimateId = estimateId,
        Beam.estimateIds = Kernel.Prelude.Just estimateIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAdvancedBookingEnabled = Kernel.Prelude.Just isAdvancedBookingEnabled,
        Beam.isScheduled = Kernel.Prelude.Just isScheduled,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.messageId = messageId,
        Beam.petCharges = Kernel.Prelude.roundToIntegral <$> petCharges,
        Beam.petChargesAmount = petCharges,
        Beam.preferSafetyPlus = Kernel.Prelude.Just preferSafetyPlus,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.searchRepeatCounter = searchRepeatCounter,
        Beam.searchRepeatType = searchRepeatType,
        Beam.serviceTierArray = Kernel.Prelude.Just serviceTierArray,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleVariant = vehicleServiceTier,
        Beam.vehicleServiceTierName = Kernel.Prelude.Just vehicleServiceTierName
      }
