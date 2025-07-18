{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.NyRegularSubscription where

import qualified Data.Text
import qualified Domain.Types.NyRegularSubscription
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.NyRegularSubscription as Beam
import qualified Storage.Queries.Location

instance FromTType' Beam.NyRegularSubscription Domain.Types.NyRegularSubscription.NyRegularSubscription where
  fromTType' (Beam.NyRegularSubscriptionT {..}) = do
    dropoffLocation' <- Storage.Queries.Location.findById (Kernel.Types.Id.Id dropoffLocationId) >>= fromMaybeM (Kernel.Types.Error.InternalError ("Failed to get dropoffLocation " <> dropoffLocationId))
    pickupLocation' <- Storage.Queries.Location.findById (Kernel.Types.Id.Id pickupLocationId) >>= fromMaybeM (Kernel.Types.Error.InternalError ("Failed to get pickupLocation " <> pickupLocationId))
    pure $
      Just
        Domain.Types.NyRegularSubscription.NyRegularSubscription
          { bppId = bppId,
            createdAt = createdAt,
            dropoffLocation = dropoffLocation',
            fixedPrice = Kernel.Types.Common.mkPrice fixedPriceCurrency <$> fixedPrice,
            fixedPriceBreakupDetails = fixedPriceBreakupDetails,
            fixedPriceExpiryDate = fixedPriceExpiryDate,
            id = Kernel.Types.Id.Id id,
            initialBppQuoteId = initialBppQuoteId,
            lastProcessedAt = lastProcessedAt,
            metadata = metadata,
            pauseEndDate = pauseEndDate,
            pauseStartDate = pauseStartDate,
            pickupLocation = pickupLocation',
            recurrenceEndDate = recurrenceEndDate,
            recurrenceRuleDays = read . Data.Text.unpack <$> recurrenceRuleDays,
            scheduledTimeOfDay = scheduledTimeOfDay,
            schedulingHash = schedulingHash,
            startDatetime = startDatetime,
            status = status,
            updatedAt = updatedAt,
            userId = Kernel.Types.Id.Id userId,
            vehicleServiceTier = vehicleServiceTier,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.NyRegularSubscription Domain.Types.NyRegularSubscription.NyRegularSubscription where
  toTType' (Domain.Types.NyRegularSubscription.NyRegularSubscription {..}) = do
    Beam.NyRegularSubscriptionT
      { Beam.bppId = bppId,
        Beam.createdAt = createdAt,
        Beam.dropoffLocationId = Kernel.Types.Id.getId $ (.id) dropoffLocation,
        Beam.fixedPrice = (.amount) <$> fixedPrice,
        Beam.fixedPriceCurrency = (.currency) <$> fixedPrice,
        Beam.fixedPriceBreakupDetails = fixedPriceBreakupDetails,
        Beam.fixedPriceExpiryDate = fixedPriceExpiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.initialBppQuoteId = initialBppQuoteId,
        Beam.lastProcessedAt = lastProcessedAt,
        Beam.metadata = metadata,
        Beam.pauseEndDate = pauseEndDate,
        Beam.pauseStartDate = pauseStartDate,
        Beam.pickupLocationId = Kernel.Types.Id.getId $ (.id) pickupLocation,
        Beam.recurrenceEndDate = recurrenceEndDate,
        Beam.recurrenceRuleDays = map show recurrenceRuleDays,
        Beam.scheduledTimeOfDay = scheduledTimeOfDay,
        Beam.schedulingHash = schedulingHash,
        Beam.startDatetime = startDatetime,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.userId = Kernel.Types.Id.getId userId,
        Beam.vehicleServiceTier = vehicleServiceTier,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
