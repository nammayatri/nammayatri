{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.KaalChakraJobRun where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.KaalChakraJobRun as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun

instance FromTType' Beam.KaalChakraJobRun Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun where
  fromTType' (Beam.KaalChakraJobRunT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun
          { batchesProcessed = batchesProcessed,
            chakra = chakra,
            createdAt = createdAt,
            endedAt = endedAt,
            eventId = Kernel.Types.Id.Id eventId,
            failedQueryName = failedQueryName,
            failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            phase = phase,
            queryOutcomes = queryOutcomes,
            retryCount = retryCount,
            schedulerJobId = schedulerJobId,
            startedAt = startedAt,
            status = status,
            tagStats = tagStats,
            updatedAt = updatedAt,
            usersProcessed = usersProcessed
          }

instance ToTType' Beam.KaalChakraJobRun Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun where
  toTType' (Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun {..}) = do
    Beam.KaalChakraJobRunT
      { Beam.batchesProcessed = batchesProcessed,
        Beam.chakra = chakra,
        Beam.createdAt = createdAt,
        Beam.endedAt = endedAt,
        Beam.eventId = Kernel.Types.Id.getId eventId,
        Beam.failedQueryName = failedQueryName,
        Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.phase = phase,
        Beam.queryOutcomes = queryOutcomes,
        Beam.retryCount = retryCount,
        Beam.schedulerJobId = schedulerJobId,
        Beam.startedAt = startedAt,
        Beam.status = status,
        Beam.tagStats = tagStats,
        Beam.updatedAt = updatedAt,
        Beam.usersProcessed = usersProcessed
      }
