{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.EDCMachineMapping where

import qualified Domain.Types.EDCMachineMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.EDCMachineMapping as Beam

instance FromTType' Beam.EDCMachineMapping Domain.Types.EDCMachineMapping.EDCMachineMapping where
  fromTType' (Beam.EDCMachineMappingT {..}) = do
    pure $
      Just
        Domain.Types.EDCMachineMapping.EDCMachineMapping
          { clientId = clientId,
            createdAt = createdAt,
            createdBy = Kernel.Types.Id.Id <$> createdBy,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            machineName = machineName,
            merchantChannelId = merchantChannelId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantKey = merchantKey,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paytmMid = paytmMid,
            personId = Kernel.Types.Id.Id personId,
            terminalId = terminalId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EDCMachineMapping Domain.Types.EDCMachineMapping.EDCMachineMapping where
  toTType' (Domain.Types.EDCMachineMapping.EDCMachineMapping {..}) = do
    Beam.EDCMachineMappingT
      { Beam.clientId = clientId,
        Beam.createdAt = createdAt,
        Beam.createdBy = Kernel.Types.Id.getId <$> createdBy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.machineName = machineName,
        Beam.merchantChannelId = merchantChannelId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantKey = merchantKey,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paytmMid = paytmMid,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.terminalId = terminalId,
        Beam.updatedAt = updatedAt
      }
