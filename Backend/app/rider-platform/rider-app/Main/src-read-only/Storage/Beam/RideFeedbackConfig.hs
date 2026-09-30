{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideFeedbackConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RideFeedbackConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RideFeedbackConfigT f = RideFeedbackConfigT
  { acknowledgement :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    actionRules :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    allowedRideStatuses :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    cooldownDays :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    createdBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    description :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    displayTrigger :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    endsAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    inputConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    isFollowUpOnly :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isSkippable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    maxShowsPerRide :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    options :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    priority :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    questionKey :: (B.C f Kernel.Prelude.Text),
    questionType :: (B.C f Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType),
    startsAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    title :: (B.C f Data.Aeson.Value),
    uiConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    version :: (B.C f Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideFeedbackConfigT where
  data PrimaryKey RideFeedbackConfigT f = RideFeedbackConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideFeedbackConfigId . id

type RideFeedbackConfig = RideFeedbackConfigT Identity

$(enableKVPG (''RideFeedbackConfigT) [('id)] [])

$(mkTableInstances (''RideFeedbackConfigT) "ride_feedback_config")
