{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.KaalChakraJobRun (module Lib.Yudhishthira.Storage.Queries.KaalChakraJobRun, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.KaalChakraJobRun as Beam
import Lib.Yudhishthira.Storage.Queries.KaalChakraJobRunExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun] -> m ())
createMany = traverse_ create

findAllByEventId :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Yudhishthira.Types.Event -> m ([Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun]))
findAllByEventId eventId = do findAllWithKV [Se.Is Beam.eventId $ Se.Eq (Kernel.Types.Id.getId eventId)]

findById ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun -> m (Maybe Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun -> m (Maybe Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchesProcessed batchesProcessed,
      Se.Set Beam.chakra chakra,
      Se.Set Beam.endedAt endedAt,
      Se.Set Beam.eventId (Kernel.Types.Id.getId eventId),
      Se.Set Beam.failedQueryName failedQueryName,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.phase phase,
      Se.Set Beam.queryOutcomes queryOutcomes,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.schedulerJobId schedulerJobId,
      Se.Set Beam.startedAt startedAt,
      Se.Set Beam.status status,
      Se.Set Beam.tagStats tagStats,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.usersProcessed usersProcessed
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
