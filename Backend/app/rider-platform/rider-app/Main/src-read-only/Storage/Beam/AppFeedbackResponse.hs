{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AppFeedbackResponse where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.AppFeedbackConfig
import qualified Domain.Types.AppFeedbackResponse
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Feedback.Types
import Tools.Beam.UtilsTH

data AppFeedbackResponseT f = AppFeedbackResponseT
  { actionResults :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    answerMediaFileIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    answerNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    answerOptionKeys :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    answerRating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    answerText :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    answerScore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    configId :: (B.C f Kernel.Prelude.Text),
    configPilotVersions :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int])),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    logicVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    placement :: (B.C f Domain.Types.AppFeedbackConfig.AppFeedbackPlacement),
    questionKey :: (B.C f Kernel.Prelude.Text),
    questionType :: (B.C f Lib.Feedback.Types.FeedbackQuestionType),
    status :: (B.C f Lib.Feedback.Types.FeedbackResponseStatus),
    subjectId :: (B.C f Kernel.Prelude.Text),
    subjectType :: (B.C f Domain.Types.AppFeedbackResponse.AppFeedbackSubjectType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppFeedbackResponseT where
  data PrimaryKey AppFeedbackResponseT f = AppFeedbackResponseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AppFeedbackResponseId . id

type AppFeedbackResponse = AppFeedbackResponseT Identity

$(enableKVPG (''AppFeedbackResponseT) [('id)] [[('personId)]])

$(mkTableInstances (''AppFeedbackResponseT) "app_feedback_response")
