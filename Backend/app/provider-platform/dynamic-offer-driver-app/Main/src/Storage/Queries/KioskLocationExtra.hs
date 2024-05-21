{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.KioskLocationExtra where

import Domain.Types.KioskLocation
import Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.KioskLocation as BeamK
import Storage.Queries.OrphanInstances.KioskLocation

-- Extra code goes here --

fetchAllKioskLocationsByMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [KioskLocation]
fetchAllKioskLocationsByMerchant _ = findAllWithKV [Se.Is BeamK.merchantId $ Se.Not $ Se.Eq ""]
