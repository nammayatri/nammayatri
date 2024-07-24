{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.WhiteListOrg where

import qualified Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.WhiteListOrg as Beam
import Storage.Queries.Transformers.WhiteListOrg

instance FromTType' Beam.WhiteListOrg Domain.Types.WhiteListOrg.WhiteListOrg where
  fromTType' (Beam.WhiteListOrgT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $
      Just
        Domain.Types.WhiteListOrg.WhiteListOrg
          { createdAt = createdAt',
            domain = domain,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            subscriberId = Kernel.Types.Id.ShortId subscriberId,
            updatedAt = updatedAt'
          }

instance ToTType' Beam.WhiteListOrg Domain.Types.WhiteListOrg.WhiteListOrg where
  toTType' (Domain.Types.WhiteListOrg.WhiteListOrg {..}) = do
    Beam.WhiteListOrgT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
