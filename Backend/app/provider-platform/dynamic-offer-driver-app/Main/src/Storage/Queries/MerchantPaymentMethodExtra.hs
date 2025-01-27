module Storage.Queries.MerchantPaymentMethodExtra where

import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantPaymentMethod as BeamMPM
import Storage.Queries.OrphanInstances.MerchantPaymentMethod ()

findAllByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m [MerchantPaymentMethod]
findAllByMerchantOpCityId (Id merchantOperatingCityId) = findAllWithOptionsKV [Se.Is BeamMPM.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc BeamMPM.priority) Nothing Nothing
