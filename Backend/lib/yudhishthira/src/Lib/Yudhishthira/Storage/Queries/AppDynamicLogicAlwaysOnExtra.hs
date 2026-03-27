{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicAlwaysOnExtra where

import Data.List (sortOn)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicAlwaysOn as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicAlwaysOn ()
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn
import Sequelize as Se

-- | Find always-on patches for (city, domain) sorted by application order.
findByMerchantOpCityAndDomainOrdered ::
  BeamFlow.BeamFlow m r =>
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m [Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn]
findByMerchantOpCityAndDomainOrdered merchantOperatingCityId domain = do
  rows <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
            Se.Is Beam.domain $ Se.Eq domain
          ]
      ]
  pure $ sortOn (.order) rows

deleteByPrimaryKey ::
  (BeamFlow.BeamFlow m r) =>
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  Int ->
  m ()
deleteByPrimaryKey merchantOperatingCityId domain version =
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.version $ Se.Eq version
        ]
    ]
