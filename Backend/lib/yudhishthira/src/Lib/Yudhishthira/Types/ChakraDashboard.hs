module Lib.Yudhishthira.Types.ChakraDashboard where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Lib.Yudhishthira.Types.KaalChakra (Chakra)
import qualified Lib.Yudhishthira.Types.KaalChakraJobRun as KCJR

-- Dashboard DTOs for the Kaal Chakra Jobs tab's run history (list + detail).
-- Shared across BPP/BAP, same as RunKaalChakraJobReq/Res in Lib.Yudhishthira.Types.
-- Kept in its own module (not Types.hs itself) because Types.KaalChakraJobRun
-- (generated) already imports Lib.Yudhishthira.Types for the Chakra/Merchant
-- phantoms on its own fields -- importing it back from Types.hs would cycle.
data ChakraRunListItem = ChakraRunListItem
  { eventId :: Text,
    chakra :: Chakra,
    phase :: KCJR.KaalChakraJobRunPhase,
    status :: KCJR.KaalChakraJobRunStatus,
    startedAt :: UTCTime,
    endedAt :: Maybe UTCTime,
    usersProcessed :: Maybe Int,
    batchesProcessed :: Maybe Int,
    retryCount :: Maybe Int,
    failureReason :: Maybe Text,
    failedQueryName :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ChakraRunListRes = ChakraRunListRes
  { runs :: [ChakraRunListItem]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ChakraRunDetailRes = ChakraRunDetailRes
  { eventId :: Text,
    chakra :: Chakra,
    phases :: [ChakraRunListItem]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ChakraJobRescheduleReq = ChakraJobRescheduleReq
  { jobId :: Text,
    scheduledAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ChakraJobCancelReq = ChakraJobCancelReq
  { jobId :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ChakraJobDeleteReq = ChakraJobDeleteReq
  { jobId :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ChakraUpcomingJob = ChakraUpcomingJob
  { jobId :: Text,
    chakra :: Chakra,
    phase :: KCJR.KaalChakraJobRunPhase,
    scheduledAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ChakraUpcomingJobsRes = ChakraUpcomingJobsRes
  { jobs :: [ChakraUpcomingJob]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets ChakraJobRescheduleReq where
  hideSecrets = identity

instance HideSecrets ChakraJobCancelReq where
  hideSecrets = identity

instance HideSecrets ChakraJobDeleteReq where
  hideSecrets = identity
