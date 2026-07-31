{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RiderPreferences where

import qualified Data.Aeson
import qualified Domain.Types.RiderPreferences
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.RiderPreferences as Beam

instance FromTType' Beam.RiderPreferences Domain.Types.RiderPreferences.RiderPreferences where
  fromTType' (Beam.RiderPreferencesT {..}) = do
    preferenceData' <- Kernel.Utils.JSON.valueToMaybe preferenceData & fromMaybeM (InternalError $ "Failed to decode preferenceData for RiderPreferences id=" <> id)
    pure $
      Just
        Domain.Types.RiderPreferences.RiderPreferences
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            preferenceData = preferenceData',
            preferenceType = preferenceType,
            riderId = Kernel.Types.Id.Id riderId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderPreferences Domain.Types.RiderPreferences.RiderPreferences where
  toTType' (Domain.Types.RiderPreferences.RiderPreferences {..}) = do
    Beam.RiderPreferencesT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.preferenceData = Data.Aeson.toJSON preferenceData,
        Beam.preferenceType = preferenceType,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.updatedAt = updatedAt
      }
