{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.KaalChakraJobRun where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun
import Tools.Beam.UtilsTH

data KaalChakraJobRunT f = KaalChakraJobRunT
  { batchesProcessed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    chakra :: (B.C f Lib.Yudhishthira.Types.Chakra),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    endedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    eventId :: (B.C f Kernel.Prelude.Text),
    failedQueryName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    failureReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    phase :: (B.C f Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRunPhase),
    queryOutcomes :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    retryCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    schedulerJobId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    startedAt :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRunStatus),
    tagStats :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    usersProcessed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int))
  }
  deriving (Generic, B.Beamable)

instance B.Table KaalChakraJobRunT where
  data PrimaryKey KaalChakraJobRunT f = KaalChakraJobRunId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = KaalChakraJobRunId . id

type KaalChakraJobRun = KaalChakraJobRunT Identity

$(enableKVPG (''KaalChakraJobRunT) [('id)] [[('eventId)]])

$(mkTableInstancesGenericSchema (''KaalChakraJobRunT) "kaal_chakra_job_run")
