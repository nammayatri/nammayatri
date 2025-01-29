{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicElement
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
