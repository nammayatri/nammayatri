{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.BulkUserCohortMappingRunExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun as DRun
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.BulkUserCohortMappingRun as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.BulkUserCohortMappingRun ()
import qualified Sequelize as Se

-- | Insert a new run row (id = dashboard runId). Caller supplies the Id.
createRun :: (BeamFlow m r) => DRun.BulkUserCohortMappingRun -> m ()
createRun = createWithKV

findByMerchantOperatingCityIdAndMaybeStatus ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.MerchantOperatingCity ->
  Maybe DRun.BulkUserCohortMappingRunStatus ->
  m [DRun.BulkUserCohortMappingRun]
findByMerchantOperatingCityIdAndMaybeStatus limit offset merchantOperatingCityId mbStatus =
  findAllWithOptionsKV
    ( case mbStatus of
        Nothing ->
          [Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)]
        Just status ->
          [ Se.And
              [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
                Se.Is Beam.status $ Se.Eq status
              ]
          ]
    )
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | Read-modify-write progress. Safe to call from the worker after each chunk.
-- 'mbErrorMessage': set on Failed / optional Running (retry) updates; cleared on Succeeded.
updateProgress ::
  (BeamFlow m r) =>
  Id DRun.BulkUserCohortMappingRun ->
  DRun.BulkUserCohortMappingRunStatus ->
  Int ->
  Maybe Int ->
  Int ->
  Int ->
  Maybe Text ->
  Maybe Text ->
  m ()
updateProgress runId status offset totalRows insertedDelta skippedDelta mbSchedulerJobId mbErrorMessage = do
  mbRun <- findOneWithKV [Se.Is Beam.id $ Se.Eq (getId runId)]
  case mbRun of
    Nothing -> pure ()
    Just existing -> do
      now <- getCurrentTime
      let newInserted = existing.rowsInserted + max 0 insertedDelta
          newSkipped = existing.rowsSkipped + max 0 skippedDelta
          newTotal = maybe existing.totalRows Just totalRows
          newJobId = maybe existing.currentSchedulerJobId Just mbSchedulerJobId
          errorSets = case status of
            DRun.Succeeded -> [Se.Set Beam.errorMessage Nothing]
            DRun.Failed -> [Se.Set Beam.errorMessage mbErrorMessage]
            _ -> case mbErrorMessage of
              Just msg -> [Se.Set Beam.errorMessage (Just msg)]
              Nothing -> []
      updateWithKV
        ( [ Se.Set Beam.status status,
            Se.Set Beam.fileOffset offset,
            Se.Set Beam.totalRows newTotal,
            Se.Set Beam.rowsInserted newInserted,
            Se.Set Beam.rowsSkipped newSkipped,
            Se.Set Beam.currentSchedulerJobId newJobId,
            Se.Set Beam.updatedAt now
          ]
            <> errorSets
        )
        [Se.Is Beam.id $ Se.Eq (getId runId)]
