{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceConfig (module Storage.Queries.MerchantServiceConfig, module ReExport) where

import qualified Domain.Types.MerchantServiceConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.MerchantServiceConfigExtra as ReExport
import Storage.Queries.Transformers.MerchantServiceConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceConfig.MerchantServiceConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantServiceConfig.MerchantServiceConfig] -> m ())
createMany = traverse_ create
