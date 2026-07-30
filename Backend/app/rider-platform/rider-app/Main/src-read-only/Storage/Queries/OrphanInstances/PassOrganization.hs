{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PassOrganization where

import qualified Domain.Types.PassOrganization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PassOrganization as Beam

instance FromTType' Beam.PassOrganization Domain.Types.PassOrganization.PassOrganization where
  fromTType' (Beam.PassOrganizationT {..}) = do
    pure $
      Just
        Domain.Types.PassOrganization.PassOrganization
          { address = address,
            createdAt = createdAt,
            depotId = depotId,
            depotPersonId = Kernel.Types.Id.Id <$> depotPersonId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            passEnum = passEnum,
            personId = Kernel.Types.Id.Id personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PassOrganization Domain.Types.PassOrganization.PassOrganization where
  toTType' (Domain.Types.PassOrganization.PassOrganization {..}) = do
    Beam.PassOrganizationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.depotId = depotId,
        Beam.depotPersonId = Kernel.Types.Id.getId <$> depotPersonId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.passEnum = passEnum,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.updatedAt = updatedAt
      }
