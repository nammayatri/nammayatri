{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WhiteListOrgExtra where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as BeamBLO
import Storage.Queries.OrphanInstances.WhiteListOrg

-- Extra code goes here --

countTotalSubscribers :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id MerchantOperatingCity -> m Int
countTotalSubscribers merchantId merchantOperatingCityId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamBLO.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is BeamBLO.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]
    <&> length
