{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.WalkLegMultimodalExtra where

import Domain.Types.Journey
import Domain.Types.WalkLegMultimodal
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.WalkLegMultimodal as Beam
import Storage.Queries.OrphanInstances.WalkLegMultimodal ()

-- Extra code goes here --

findAllByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m [Domain.Types.WalkLegMultimodal.WalkLegMultimodal]
findAllByJourneyId journeyId =
  findAllWithKVAndConditionalDB
    [Se.Is Beam.journeyId $ Se.Eq (Just journeyId.getId)]
    Nothing
