{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MerchantClientConfigExtra where

import Domain.Types.MerchantClientConfig
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Version (DeviceType)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantClientConfig as BeamMCC
import Storage.Queries.OrphanInstances.MerchantClientConfig ()

-- Extra code goes here --
findByPackageOSAndService :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ClientService -> Maybe DeviceType -> Text -> m (Maybe MerchantClientConfig)
findByPackageOSAndService service clientOS packageId =
  findOneWithKV [Se.And [Se.Is BeamMCC.serviceName $ Se.Eq service, Se.Is BeamMCC.packageId $ Se.Eq packageId, Se.Is BeamMCC.clientOS $ Se.Eq clientOS]]
