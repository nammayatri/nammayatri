{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.NammaTagExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.NammaTag as Beam
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.NammaTag
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag
import Sequelize as Se

-- Extra code goes here --
findAllByApplicationEvent :: (BeamFlow.BeamFlow m r) => Lib.Yudhishthira.Types.ApplicationEvent -> m [Lib.Yudhishthira.Types.NammaTag.NammaTag]
findAllByApplicationEvent event = do findAllWithKV [Se.And [Se.Is Beam.event $ Se.Eq (Just event)]]

findAllByChakra :: (BeamFlow.BeamFlow m r) => Lib.Yudhishthira.Types.Chakra -> m [Lib.Yudhishthira.Types.NammaTag.NammaTag]
findAllByChakra chakra = do findAllWithKV [Se.And [Se.Is Beam.chakra $ Se.Eq (Just chakra)]]
