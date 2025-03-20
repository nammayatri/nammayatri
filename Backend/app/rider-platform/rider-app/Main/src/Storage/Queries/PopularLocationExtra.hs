module Storage.Queries.PopularLocationExtra where

import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PopularLocation as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize
import Storage.Beam.PopularLocation as Beam
import Storage.Queries.OrphanInstances.PopularLocation ()

-- Extra code goes here --

findAllByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [Domain.PopularLocation]
findAllByMerchantOperatingCityId (Id merchantOperatingCityId) = do
  findAllWithKV
    [ Is Beam.merchantOperatingCityId $ Eq merchantOperatingCityId
    ]
