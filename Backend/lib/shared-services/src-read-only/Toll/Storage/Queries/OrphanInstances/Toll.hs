{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Toll.Storage.Queries.OrphanInstances.Toll where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Toll.Domain.Types.Toll
import qualified Toll.Storage.Beam.Toll as Beam

instance FromTType' Beam.Toll Toll.Domain.Types.Toll.Toll where
  fromTType' (Beam.TollT {..}) = do
    pure $
      Just
        Toll.Domain.Types.Toll.Toll
          { id = Kernel.Types.Id.Id id,
            isAutoRickshawAllowed = isAutoRickshawAllowed,
            isTwoWheelerAllowed = isTwoWheelerAllowed,
            name = name,
            price = Kernel.Types.Common.mkPrice currency price,
            tollEndGates = tollEndGates,
            tollStartGates = tollStartGates,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Toll Toll.Domain.Types.Toll.Toll where
  toTType' (Toll.Domain.Types.Toll.Toll {..}) = do
    Beam.TollT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.isAutoRickshawAllowed = isAutoRickshawAllowed,
        Beam.isTwoWheelerAllowed = isTwoWheelerAllowed,
        Beam.name = name,
        Beam.currency = ((Kernel.Prelude.Just . (.currency))) price,
        Beam.price = (.amount) price,
        Beam.tollEndGates = tollEndGates,
        Beam.tollStartGates = tollStartGates,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
