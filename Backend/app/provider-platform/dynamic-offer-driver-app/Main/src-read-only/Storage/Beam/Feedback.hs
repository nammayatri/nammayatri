{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Feedback where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FeedbackT f
    = FeedbackT {badge :: (B.C f Kernel.Prelude.Text),
                 badgeKey :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                 driverId :: (B.C f Kernel.Prelude.Text),
                 id :: (B.C f Kernel.Prelude.Text),
                 rating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                 rideId :: (B.C f Kernel.Prelude.Text),
                 merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                 merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table FeedbackT
    where data PrimaryKey FeedbackT f = FeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FeedbackId . id
type Feedback = FeedbackT Identity

$(enableKVPG (''FeedbackT) [('id)] [])

$(mkTableInstances (''FeedbackT) "feedback")

