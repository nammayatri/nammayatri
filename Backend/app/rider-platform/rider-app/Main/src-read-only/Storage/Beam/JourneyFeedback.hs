{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.JourneyFeedback where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data JourneyFeedbackT f
    = JourneyFeedbackT {additionalFeedBack :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        journeyId :: (B.C f Kernel.Prelude.Text),
                        rating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                        riderId :: (B.C f Kernel.Prelude.Text),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table JourneyFeedbackT
    where data PrimaryKey JourneyFeedbackT f = JourneyFeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = JourneyFeedbackId . journeyId
type JourneyFeedback = JourneyFeedbackT Identity

$(enableKVPG (''JourneyFeedbackT) [('journeyId)] [[('riderId)]])

$(mkTableInstances (''JourneyFeedbackT) "journey_feedback")

