{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MerchantServiceConfig (module Storage.Queries.MerchantServiceConfig, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.MerchantServiceConfigExtra as ReExport
import qualified Domain.Types.MerchantServiceConfig
import qualified Storage.Beam.MerchantServiceConfig as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceConfig.MerchantServiceConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantServiceConfig.MerchantServiceConfig] -> m ())
createMany = traverse_ create
findAllByMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                    (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.MerchantServiceConfig.MerchantServiceConfig]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]



