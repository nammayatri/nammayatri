{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.KaalChakraJobRunExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.KaalChakraJobRun as Beam
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.KaalChakraJobRun
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun as DKCJR
import qualified Sequelize as Se

-- Dashboard list query: merchant+city scoped, with optional chakra/status/
-- date-range filters and pagination. kaal_chakra_job_run is in disableForKV
-- (see the "disable-kv-kaal-chakra-job-run" migration), so this reads
-- straight from Postgres regardless of secondary keys — merchantOperatingCityId
-- and status can never be KV secondary keys anyway.
findAllByFilters ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Id LYT.MerchantOperatingCity ->
  Maybe LYT.Chakra ->
  Maybe DKCJR.KaalChakraJobRunStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Int ->
  Int ->
  m [DKCJR.KaalChakraJobRun]
findAllByFilters merchantOperatingCityId mbChakra mbStatus mbFrom mbTo limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)]
            <> [Se.Is Beam.chakra $ Se.Eq chakra | Just chakra <- [mbChakra]]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
            <> [Se.Is Beam.startedAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
            <> [Se.Is Beam.startedAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
        )
    ]
    (Se.Desc Beam.startedAt)
    (Just limit)
    (Just offset)

-- A scheduler job keeps its id across batches (ReSchedule reuses it), so any row
-- here means that job has started at least one batch.
findAllBySchedulerJobId ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  m [DKCJR.KaalChakraJobRun]
findAllBySchedulerJobId schedulerJobId =
  findAllWithKV [Se.Is Beam.schedulerJobId $ Se.Eq (Just schedulerJobId)]
