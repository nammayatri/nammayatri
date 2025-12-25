{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.TimeBoundConfigExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.TimeBoundConfig as Beam
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.TimeBoundConfig ()
import Lib.Yudhishthira.Types
import qualified Sequelize as Se

-- Extra code goes here --
delete :: BeamFlow.BeamFlow m r => Id MerchantOperatingCity -> LogicDomain -> Text -> m ()
delete cityId timeBoundDomain name = deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq cityId.getId, Se.Is Beam.name $ Se.Eq name, Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain]]
