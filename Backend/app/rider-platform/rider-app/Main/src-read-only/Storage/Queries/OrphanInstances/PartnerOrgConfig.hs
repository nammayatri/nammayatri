{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.PartnerOrgConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.Transformers.PartnerOrgConfig
import qualified Domain.Types.PartnerOrgConfig
import qualified Storage.Beam.PartnerOrgConfig as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.PartnerOrgConfig Domain.Types.PartnerOrgConfig.PartnerOrgConfig
    where fromTType' (Beam.PartnerOrgConfigT {..}) = do {config' <- getPOrgConfigFromTypeAndJson configJSON configType;
                                                         pure $ Just Domain.Types.PartnerOrgConfig.PartnerOrgConfig{config = config', createdAt = createdAt, partnerOrgId = Kernel.Types.Id.Id partnerOrgId, updatedAt = updatedAt}}
instance ToTType' Beam.PartnerOrgConfig Domain.Types.PartnerOrgConfig.PartnerOrgConfig
    where toTType' (Domain.Types.PartnerOrgConfig.PartnerOrgConfig {..}) = do Beam.PartnerOrgConfigT{Beam.configJSON = snd $ getTypeAndJSONFromPOrgConfig config,
                                                                                                     Beam.configType = fst $ getTypeAndJSONFromPOrgConfig config,
                                                                                                     Beam.createdAt = createdAt,
                                                                                                     Beam.partnerOrgId = Kernel.Types.Id.getId partnerOrgId,
                                                                                                     Beam.updatedAt = updatedAt}



