{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyFeedback where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data JourneyFeedbackT f = JourneyFeedbackT
  { additionalFeedBack :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    journeyId :: B.C f Kernel.Prelude.Text,
    rating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    riderId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyFeedbackT where
  data PrimaryKey JourneyFeedbackT f = JourneyFeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyFeedbackId . journeyId

type JourneyFeedback = JourneyFeedbackT Identity

$(enableKVPG ''JourneyFeedbackT ['journeyId] [['riderId]])

$(mkTableInstances ''JourneyFeedbackT "journey_feedback")
