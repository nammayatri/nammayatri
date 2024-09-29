{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TimeBoundConfigExtra where

import qualified Domain.Types.MerchantOperatingCity as MOC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.TimeBoundConfig as Beam
import Storage.Queries.OrphanInstances.TimeBoundConfig

-- Extra code goes here --
delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MOC.MerchantOperatingCity -> LogicDomain -> Text -> m ()
delete cityId timeBoundDomain name = deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq cityId.getId, Se.Is Beam.name $ Se.Eq name, Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain]]
