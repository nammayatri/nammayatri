{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantPaymentMethodExtra where

import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantPaymentMethod as BeamMPM
import Storage.Queries.OrphanInstances.MerchantPaymentMethod

-- Extra code goes here --
findAllByMerchantOperatingCityId :: KvDbFlow m r => Id MerchantOperatingCity -> m [MerchantPaymentMethod]
findAllByMerchantOperatingCityId (Id merchantOperatingCityId) = findAllWithOptionsKV [Se.Is BeamMPM.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc BeamMPM.priority) Nothing Nothing
