{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SvpJourney where

import qualified Domain.Types.SvpJourney
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SvpJourney as Beam

instance FromTType' Beam.SvpJourney Domain.Types.SvpJourney.SvpJourney where
  fromTType' (Beam.SvpJourneyT {..}) = do
    pure $
      Just
        Domain.Types.SvpJourney.SvpJourney
          { createdAt = createdAt,
            currency = currency,
            entryStationCode = entryStationCode,
            entryTime = entryTime,
            exitStationCode = exitStationCode,
            exitTime = exitTime,
            fareCharged = fareCharged,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            riderId = Kernel.Types.Id.Id riderId,
            status = status,
            tktSlNo = tktSlNo,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SvpJourney Domain.Types.SvpJourney.SvpJourney where
  toTType' (Domain.Types.SvpJourney.SvpJourney {..}) = do
    Beam.SvpJourneyT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.entryStationCode = entryStationCode,
        Beam.entryTime = entryTime,
        Beam.exitStationCode = exitStationCode,
        Beam.exitTime = exitTime,
        Beam.fareCharged = fareCharged,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.status = status,
        Beam.tktSlNo = tktSlNo,
        Beam.updatedAt = updatedAt
      }
