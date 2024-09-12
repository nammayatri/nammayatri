{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogic as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogic
import qualified Lib.Yudhishthira.Types
import Sequelize as Se

-- Extra code goes here --
delete :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> TimeBound -> m ()
delete cityId domain timeBounds = deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId cityId), Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.timeBounds $ Se.Eq (Just timeBounds)]]
