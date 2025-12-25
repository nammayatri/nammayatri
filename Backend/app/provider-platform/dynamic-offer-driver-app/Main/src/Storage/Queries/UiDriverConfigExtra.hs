{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.UiDriverConfigExtra where

import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.UiDriverConfig as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types as YType
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified Sequelize as Se
import qualified Storage.Beam.UiDriverConfig as Beam
import Storage.Queries.OrphanInstances.UiDriverConfig

-- Extra code goes here --
findUIConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => YType.UiConfigRequest -> Id MerchantOperatingCity -> m (Maybe Domain.UiDriverConfig)
findUIConfig YType.UiConfigRequest {..} merchantOperatingCityId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.os $ Se.Eq os,
          Se.Is Beam.platform $ Se.Eq platform
        ]
    ]

$(YTH.generateGenericDefault ''Domain.UiDriverConfig)
