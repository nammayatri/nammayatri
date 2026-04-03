{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.PlaceBasedServiceConfig (module Storage.Queries.PlaceBasedServiceConfig, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.PlaceBasedServiceConfigExtra as ReExport
import qualified Domain.Types.PlaceBasedServiceConfig



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig] -> m ())
createMany = traverse_ create



