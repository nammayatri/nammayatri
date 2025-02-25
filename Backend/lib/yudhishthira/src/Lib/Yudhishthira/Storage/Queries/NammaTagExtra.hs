module Lib.Yudhishthira.Storage.Queries.NammaTagExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.NammaTag as Beam
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.NammaTag ()
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag
import Sequelize as Se

-- Extra code goes here --
findAllByChakra :: BeamFlow.BeamFlow m r => Lib.Yudhishthira.Types.Chakra -> m [Lib.Yudhishthira.Types.NammaTag.NammaTag]
findAllByChakra chakra = do findAllWithKV [Se.And [Se.Is Beam.chakra $ Se.Eq (Just chakra)]]

findAll :: BeamFlow.BeamFlow m r => m [Lib.Yudhishthira.Types.NammaTag.NammaTag]
findAll = findAllWithKV @Beam.NammaTagT []
