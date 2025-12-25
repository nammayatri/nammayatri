{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantClientConfigExtra where

import Domain.Types.MerchantClientConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Version
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantClientConfig as BeamMCC
import Storage.Queries.OrphanInstances.MerchantClientConfig

-- Extra code goes here --
findByPackageOSAndService :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ClientService -> Maybe DeviceType -> Text -> m (Maybe MerchantClientConfig)
findByPackageOSAndService service clientOS packageId =
  findOneWithKV [Se.And [Se.Is BeamMCC.serviceName $ Se.Eq service, Se.Is BeamMCC.packageId $ Se.Eq packageId, Se.Is BeamMCC.clientOS $ Se.Eq clientOS]]
