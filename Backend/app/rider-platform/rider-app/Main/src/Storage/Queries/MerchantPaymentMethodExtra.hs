module Storage.Queries.MerchantPaymentMethodExtra where

import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantPaymentMethod as BeamMPM
import Storage.Queries.OrphanInstances.MerchantPaymentMethod ()

-- Extra code goes here --
findAllByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [MerchantPaymentMethod]
findAllByMerchantOperatingCityId (Id merchantOperatingCityId) = findAllWithOptionsKV [Se.Is BeamMPM.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc BeamMPM.priority) Nothing Nothing

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantPaymentMethod -> m (Maybe MerchantPaymentMethod)
findById (Id merchantPaymentMethodId) = findOneWithKV [Se.Is BeamMPM.id $ Se.Eq merchantPaymentMethodId]
