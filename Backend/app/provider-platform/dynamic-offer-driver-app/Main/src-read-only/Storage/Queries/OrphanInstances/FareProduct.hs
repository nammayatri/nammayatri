{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareProduct where

import qualified Domain.Types.FareProduct
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareProduct as Beam

instance FromTType' Beam.FareProduct Domain.Types.FareProduct.FareProduct where
  fromTType' (Beam.FareProductT {..}) = do
    pure $
      Just
        Domain.Types.FareProduct.FareProduct
          { area = area,
            disableRecompute = disableRecompute,
            disableTollRecompute = disableTollRecompute,
            enabled = enabled,
            farePolicyId = Kernel.Types.Id.Id farePolicyId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            searchSource = searchSource,
            timeBounds = timeBounds,
            tripCategory = tripCategory,
            vehicleServiceTier = vehicleVariant
          }

instance ToTType' Beam.FareProduct Domain.Types.FareProduct.FareProduct where
  toTType' (Domain.Types.FareProduct.FareProduct {..}) = do
    Beam.FareProductT
      { Beam.area = area,
        Beam.disableRecompute = disableRecompute,
        Beam.disableTollRecompute = disableTollRecompute,
        Beam.enabled = enabled,
        Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.searchSource = searchSource,
        Beam.timeBounds = timeBounds,
        Beam.tripCategory = tripCategory,
        Beam.vehicleVariant = vehicleServiceTier
      }
