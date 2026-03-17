{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SavedTrip where

import qualified Domain.Types.SavedTrip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SavedTrip as Beam

instance FromTType' Beam.SavedTrip Domain.Types.SavedTrip.SavedTrip where
  fromTType' (Beam.SavedTripT {..}) = do
    pure $
      Just
        Domain.Types.SavedTrip.SavedTrip
          { id = Kernel.Types.Id.Id id,
            riderId = Kernel.Types.Id.Id riderId,
            name = name,
            originLat = originLat,
            originLon = originLon,
            originAddress = originAddress,
            destinationLat = destinationLat,
            destinationLon = destinationLon,
            destinationAddress = destinationAddress,
            timeMode = timeMode,
            targetTime = targetTime,
            targetTimeOfDay = Kernel.Prelude.fmap secondsToTimeOfDay targetTimeOfDaySeconds,
            bufferMinutes = bufferMinutes,
            recurrence = recurrence,
            customDays = customDays,
            notifyBeforeMinutes = notifyBeforeMinutes,
            isActive = isActive,
            lastComputedDeparture = lastComputedDeparture,
            lastNotifiedAt = lastNotifiedAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SavedTrip Domain.Types.SavedTrip.SavedTrip where
  toTType' (Domain.Types.SavedTrip.SavedTrip {..}) = do
    Beam.SavedTripT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.name = name,
        Beam.originLat = originLat,
        Beam.originLon = originLon,
        Beam.originAddress = originAddress,
        Beam.destinationLat = destinationLat,
        Beam.destinationLon = destinationLon,
        Beam.destinationAddress = destinationAddress,
        Beam.timeMode = timeMode,
        Beam.targetTime = targetTime,
        Beam.targetTimeOfDaySeconds = Kernel.Prelude.fmap timeOfDayToSeconds targetTimeOfDay,
        Beam.bufferMinutes = bufferMinutes,
        Beam.recurrence = recurrence,
        Beam.customDays = customDays,
        Beam.notifyBeforeMinutes = notifyBeforeMinutes,
        Beam.isActive = isActive,
        Beam.lastComputedDeparture = lastComputedDeparture,
        Beam.lastNotifiedAt = lastNotifiedAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }

secondsToTimeOfDay :: Int -> TimeOfDay
secondsToTimeOfDay secs =
  let h = secs `div` 3600
      m = (secs `mod` 3600) `div` 60
      s = secs `mod` 60
   in TimeOfDay h m (fromIntegral s)

timeOfDayToSeconds :: TimeOfDay -> Int
timeOfDayToSeconds (TimeOfDay h m s) = h * 3600 + m * 60 + round s
