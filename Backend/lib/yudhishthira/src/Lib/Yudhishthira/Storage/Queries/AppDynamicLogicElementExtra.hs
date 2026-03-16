{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicElement ()
import qualified Lib.Yudhishthira.Types
import Sequelize as Se

-- Extra code goes here --

deleteByDomainAndVersion ::
  (BeamFlow.BeamFlow m r) =>
  Lib.Yudhishthira.Types.LogicDomain ->
  Kernel.Prelude.Int ->
  m ()
deleteByDomainAndVersion domain version = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.version $ Se.Eq version
        ]
    ]
