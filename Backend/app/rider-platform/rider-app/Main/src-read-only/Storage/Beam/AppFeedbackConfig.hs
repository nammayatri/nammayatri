{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AppFeedbackConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Feedback.Types
import Tools.Beam.UtilsTH

data AppFeedbackConfigT f = AppFeedbackConfigT
  { acknowledgement :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    actionRules :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    bypassCap :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    cooldownDays :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    displayTrigger :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    iconUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    inputConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    isFollowUpOnly :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isSkippable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    options :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    placements :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    priority :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    questionKey :: (B.C f Kernel.Prelude.Text),
    questionType :: (B.C f Lib.Feedback.Types.FeedbackQuestionType),
    subjectCondition :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    title :: (B.C f Data.Aeson.Value),
    uiConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppFeedbackConfigT where
  data PrimaryKey AppFeedbackConfigT f = AppFeedbackConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AppFeedbackConfigId . id

type AppFeedbackConfig = AppFeedbackConfigT Identity

$(enableKVPG (''AppFeedbackConfigT) [('id)] [])

$(mkTableInstances (''AppFeedbackConfigT) "app_feedback_config")
