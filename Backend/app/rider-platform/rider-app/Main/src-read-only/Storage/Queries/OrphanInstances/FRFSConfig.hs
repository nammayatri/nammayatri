{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSConfig where

import qualified Domain.Types.FRFSConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Distance
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSConfig as Beam

instance FromTType' Beam.FRFSConfig Domain.Types.FRFSConfig.FRFSConfig where
  fromTType' (Beam.FRFSConfigT {..}) = do
    pure $
      Just
        Domain.Types.FRFSConfig.FRFSConfig
          { bookingEndTime = bookingEndTime,
            bookingStartTime = bookingStartTime,
            busStationTtl = Kernel.Prelude.fromMaybe (Kernel.Types.Time.Seconds 1800) busStationTtl,
            cancellationReasonId = cancellationReasonId,
            customDates = customDates,
            customEndTime = customEndTime,
            discount = discount,
            freeTicketInterval = freeTicketInterval,
            isCancellationAllowed = Kernel.Prelude.fromMaybe True isCancellationAllowed,
            isEventOngoing = isEventOngoing,
            maxFreeTicketCashback = maxFreeTicketCashback,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            metroStationTtl = metroStationTtl,
            oneWayTicketLimit = oneWayTicketLimit,
            providerId = providerId,
            providerName = providerName,
            radius = Kernel.Prelude.fromMaybe (Kernel.Types.Distance.Meters 3000) radius,
            roundTripTicketLimit = roundTripTicketLimit,
            straightLineDistance = Kernel.Prelude.fromMaybe (Kernel.Types.Distance.Meters 5000) straightLineDistance,
            validTillSeconds = Kernel.Prelude.fromMaybe (Kernel.Types.Time.Seconds 300) validTillSeconds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSConfig Domain.Types.FRFSConfig.FRFSConfig where
  toTType' (Domain.Types.FRFSConfig.FRFSConfig {..}) = do
    Beam.FRFSConfigT
      { Beam.bookingEndTime = bookingEndTime,
        Beam.bookingStartTime = bookingStartTime,
        Beam.busStationTtl = Kernel.Prelude.Just busStationTtl,
        Beam.cancellationReasonId = cancellationReasonId,
        Beam.customDates = customDates,
        Beam.customEndTime = customEndTime,
        Beam.discount = discount,
        Beam.freeTicketInterval = freeTicketInterval,
        Beam.isCancellationAllowed = Kernel.Prelude.Just isCancellationAllowed,
        Beam.isEventOngoing = isEventOngoing,
        Beam.maxFreeTicketCashback = maxFreeTicketCashback,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.metroStationTtl = metroStationTtl,
        Beam.oneWayTicketLimit = oneWayTicketLimit,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.radius = Kernel.Prelude.Just radius,
        Beam.roundTripTicketLimit = roundTripTicketLimit,
        Beam.straightLineDistance = Kernel.Prelude.Just straightLineDistance,
        Beam.validTillSeconds = Kernel.Prelude.Just validTillSeconds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
