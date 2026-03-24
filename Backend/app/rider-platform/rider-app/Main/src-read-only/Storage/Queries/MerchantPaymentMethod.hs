{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MerchantPaymentMethod (module Storage.Queries.MerchantPaymentMethod, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.MerchantPaymentMethodExtra as ReExport
import Storage.Queries.Transformers.MerchantPaymentMethod
import qualified Domain.Types.MerchantPaymentMethod



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod] -> m ())
createMany = traverse_ create



