{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.StationsExtraInformation where

import qualified Domain.Types.StationsExtraInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Storage.Beam.StationsExtraInformation as Beam

instance FromTType' Beam.StationsExtraInformation Domain.Types.StationsExtraInformation.StationsExtraInformation where
  fromTType' (Beam.StationsExtraInformationT {..}) = do
    pure $
      Just
        Domain.Types.StationsExtraInformation.StationsExtraInformation
          { address = address,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            stationId = stationId,
            suggestedDestinations = Kernel.Utils.Text.decodeFromText =<< suggestedDestinations,
            updatedAt = updatedAt
          }

instance ToTType' Beam.StationsExtraInformation Domain.Types.StationsExtraInformation.StationsExtraInformation where
  toTType' (Domain.Types.StationsExtraInformation.StationsExtraInformation {..}) = do
    Beam.StationsExtraInformationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.stationId = stationId,
        Beam.suggestedDestinations = Kernel.Utils.Text.encodeToText <$> suggestedDestinations,
        Beam.updatedAt = updatedAt
      }
