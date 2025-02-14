{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.UiRiderConfigExtra where

import qualified Domain.Types.UiRiderConfig as URC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types as LYT
import qualified Sequelize as Se
import Storage.Beam.UiRiderConfig as Beam
import Storage.Queries.OrphanInstances.UiRiderConfig

getRiderConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => LYT.UiConfigRequest -> Maybe Text -> m (Maybe URC.UiRiderConfig)
getRiderConfig req merchantOperatingCityId = do
  findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is Beam.os $ Se.Eq req.os, Se.Is Beam.language $ Se.Eq req.language]]
