{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DepotManager where

import qualified Domain.Types.DepotManager
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DepotManager as Beam

instance FromTType' Beam.DepotManager Domain.Types.DepotManager.DepotManager where
  fromTType' (Beam.DepotManagerT {..}) = do
    pure $
      Just
        Domain.Types.DepotManager.DepotManager
          { createdAt = createdAt,
            depotCode = Kernel.Types.Id.Id depotCode,
            enabled = enabled,
            isAdmin = isAdmin,
            isBlockAllowed = isBlockAllowed,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DepotManager Domain.Types.DepotManager.DepotManager where
  toTType' (Domain.Types.DepotManager.DepotManager {..}) = do
    Beam.DepotManagerT
      { Beam.createdAt = createdAt,
        Beam.depotCode = Kernel.Types.Id.getId depotCode,
        Beam.enabled = enabled,
        Beam.isAdmin = isAdmin,
        Beam.isBlockAllowed = isBlockAllowed,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.updatedAt = updatedAt
      }
