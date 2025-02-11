{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyLegsFeedbacks where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data JourneyLegsFeedbacksT f = JourneyLegsFeedbacksT
  { isExperienceGood :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyId :: B.C f Kernel.Prelude.Text,
    legOrder :: B.C f Kernel.Prelude.Int,
    travelMode :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.MultimodalTravelMode),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyLegsFeedbacksT where
  data PrimaryKey JourneyLegsFeedbacksT f = JourneyLegsFeedbacksId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int) deriving (Generic, B.Beamable)
  primaryKey = JourneyLegsFeedbacksId <$> journeyId <*> legOrder

type JourneyLegsFeedbacks = JourneyLegsFeedbacksT Identity

$(enableKVPG ''JourneyLegsFeedbacksT ['journeyId, 'legOrder] [])

$(mkTableInstances ''JourneyLegsFeedbacksT "journey_legs_feedbacks")
