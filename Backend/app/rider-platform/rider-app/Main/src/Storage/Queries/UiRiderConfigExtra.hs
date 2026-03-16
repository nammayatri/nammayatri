{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.UiRiderConfigExtra where

import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.UiRiderConfig as URC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified Sequelize as Se
import Storage.Beam.UiRiderConfig as Beam
import Storage.Queries.OrphanInstances.UiRiderConfig ()

getUiConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => LYT.UiConfigRequest -> Id MerchantOperatingCity -> m (Maybe URC.UiRiderConfig)
getUiConfig LYT.UiConfigRequest {..} merchantOperatingCityId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.os $ Se.Eq os,
          Se.Is Beam.platform $ Se.Eq platform
        ]
    ]

$(YTH.generateGenericDefault ''URC.UiRiderConfig) -- TODO add defaults
