{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideFeedbackResponse where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RideFeedbackConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Feedback.Types
import Tools.Beam.UtilsTH

data RideFeedbackResponseT f = RideFeedbackResponseT
  { actionResults :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    answerMediaFileIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    answerNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    answerOptionKeys :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    answerRating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    answerText :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    answerScore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    bookingId :: (B.C f Kernel.Prelude.Text),
    configId :: (B.C f Kernel.Prelude.Text),
    configPilotVersions :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int])),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    id :: (B.C f Kernel.Prelude.Text),
    logicVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    questionKey :: (B.C f Kernel.Prelude.Text),
    questionType :: (B.C f Lib.Feedback.Types.FeedbackQuestionType),
    rideId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    secondsIntoRide :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    secondsSinceBooking :: (B.C f Kernel.Prelude.Int),
    stage :: (B.C f Domain.Types.RideFeedbackConfig.RideFeedbackStage),
    status :: (B.C f Lib.Feedback.Types.FeedbackResponseStatus),
    subjectId :: (B.C f Kernel.Prelude.Text),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideFeedbackResponseT where
  data PrimaryKey RideFeedbackResponseT f = RideFeedbackResponseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideFeedbackResponseId . id

type RideFeedbackResponse = RideFeedbackResponseT Identity

$(enableKVPG (''RideFeedbackResponseT) [('id)] [[('transactionId)]])

$(mkTableInstances (''RideFeedbackResponseT) "ride_feedback_response")
