{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SearchTry where

import qualified Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SearchTry as Beam
import qualified Storage.Queries.Transformers.SearchRequestForDriver
import Storage.Queries.Transformers.SearchTry

instance FromTType' Beam.SearchTry Domain.Types.SearchTry.SearchTry where
  fromTType' (Beam.SearchTryT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.SearchRequestForDriver.getMerchantOpCId merchantOperatingCityId merchantId requestId
    pure $
      Just
        Domain.Types.SearchTry.SearchTry
          { baseFare = baseFare,
            createdAt = createdAt,
            customerExtraFee = customerExtraFee,
            estimateId = estimateId,
            estimateIds = fromMaybe [estimateId] estimateIds,
            id = Kernel.Types.Id.Id id,
            isScheduled = fromMaybe Kernel.Prelude.False isScheduled,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            messageId = messageId,
            requestId = Kernel.Types.Id.Id requestId,
            searchRepeatCounter = searchRepeatCounter,
            searchRepeatType = searchRepeatType,
            startTime = startTime,
            status = status,
            tripCategory = getTripCategory tripCategory,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleServiceTier = vehicleVariant,
            vehicleServiceTierName = fromMaybe (Kernel.Prelude.show vehicleVariant) vehicleServiceTierName
          }

instance ToTType' Beam.SearchTry Domain.Types.SearchTry.SearchTry where
  toTType' (Domain.Types.SearchTry.SearchTry {..}) = do
    Beam.SearchTryT
      { Beam.baseFare = baseFare,
        Beam.createdAt = createdAt,
        Beam.customerExtraFee = customerExtraFee,
        Beam.estimateId = estimateId,
        Beam.estimateIds = Kernel.Prelude.Just estimateIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isScheduled = Kernel.Prelude.Just isScheduled,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.messageId = messageId,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.searchRepeatCounter = searchRepeatCounter,
        Beam.searchRepeatType = searchRepeatType,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleVariant = vehicleServiceTier,
        Beam.vehicleServiceTierName = Kernel.Prelude.Just vehicleServiceTierName
      }
