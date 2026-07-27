{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.SapJournalEntryExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.SapJournalEntry as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.SapJournalEntry as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.SapJournalEntry
import qualified Sequelize as Se

findLatestBatchId ::
  (BeamFlow m r) =>
  m (Maybe Text)
findLatestBatchId = do
  mbEntry <- findAllWithOptionsKV [] (Se.Desc Beam.createdAt) (Just 1) Nothing
  pure $ case mbEntry of
    [entry] -> Just (entry :: Domain.SapJournalEntry).batchId
    _ -> Nothing
