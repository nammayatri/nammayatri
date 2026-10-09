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
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    displayTrigger :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    inputConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    isFollowUpOnly :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isSkippable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    options :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    priority :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    questionKey :: (B.C f Kernel.Prelude.Text),
    questionType :: (B.C f Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType),
    title :: (B.C f Data.Aeson.Value),
    uiConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideFeedbackConfigT where
  data PrimaryKey RideFeedbackConfigT f = RideFeedbackConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideFeedbackConfigId . id

type RideFeedbackConfig = RideFeedbackConfigT Identity

$(enableKVPG (''RideFeedbackConfigT) [('id)] [])

$(mkTableInstances (''RideFeedbackConfigT) "ride_feedback_config")
