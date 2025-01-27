{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRolloutExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicRollout ()
import qualified Lib.Yudhishthira.Types
import Sequelize as Se

-- Extra code goes here --
delete :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
delete cityId domain = deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId cityId), Se.Is Beam.domain $ Se.Eq domain]]
