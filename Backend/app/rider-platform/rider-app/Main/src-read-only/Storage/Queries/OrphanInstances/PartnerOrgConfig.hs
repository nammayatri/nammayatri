{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PartnerOrgConfig where

import qualified Domain.Types.PartnerOrgConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PartnerOrgConfig as Beam
import Storage.Queries.Transformers.PartnerOrgConfig

instance FromTType' Beam.PartnerOrgConfig Domain.Types.PartnerOrgConfig.PartnerOrgConfig where
  fromTType' (Beam.PartnerOrgConfigT {..}) = do
    config' <- getPOrgConfigFromTypeAndJson configJSON configType
    pure $ Just Domain.Types.PartnerOrgConfig.PartnerOrgConfig {config = config', createdAt = createdAt, partnerOrgId = Kernel.Types.Id.Id partnerOrgId, updatedAt = updatedAt}

instance ToTType' Beam.PartnerOrgConfig Domain.Types.PartnerOrgConfig.PartnerOrgConfig where
  toTType' (Domain.Types.PartnerOrgConfig.PartnerOrgConfig {..}) = do
    Beam.PartnerOrgConfigT
      { Beam.configJSON = snd $ getTypeAndJSONFromPOrgConfig config,
        Beam.configType = fst $ getTypeAndJSONFromPOrgConfig config,
        Beam.createdAt = createdAt,
        Beam.partnerOrgId = Kernel.Types.Id.getId partnerOrgId,
        Beam.updatedAt = updatedAt
      }
