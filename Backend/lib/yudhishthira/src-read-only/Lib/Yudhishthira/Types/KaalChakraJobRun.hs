{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.KaalChakraJobRun where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Yudhishthira.Types.Common
import qualified Lib.Yudhishthira.Types.KaalChakra
import qualified Tools.Beam.UtilsTH

data KaalChakraJobRun = KaalChakraJobRun
  { batchesProcessed :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    chakra :: Lib.Yudhishthira.Types.KaalChakra.Chakra,
    createdAt :: Kernel.Prelude.UTCTime,
    endedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    eventId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.Common.Event,
    failedQueryName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRun,
    merchantId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.Common.MerchantOperatingCity,
    phase :: Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRunPhase,
    queryOutcomes :: Kernel.Prelude.Maybe Data.Aeson.Value,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    schedulerJobId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startedAt :: Kernel.Prelude.UTCTime,
    status :: Lib.Yudhishthira.Types.KaalChakraJobRun.KaalChakraJobRunStatus,
    tagStats :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime,
    usersProcessed :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data KaalChakraJobRunPhase = FetchUserData | UpdateTags deriving (Generic, (Show), (Read), (Eq), (Ord), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

data KaalChakraJobRunStatus = Running | Completed | Failed | Cancelled deriving (Generic, (Show), (Read), (Eq), (Ord), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''KaalChakraJobRunPhase))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''KaalChakraJobRunPhase))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''KaalChakraJobRunStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''KaalChakraJobRunStatus))
