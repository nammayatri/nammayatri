{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FeedbackBadge where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FeedbackBadgeT f
    = FeedbackBadgeT {badge :: (B.C f Kernel.Prelude.Text),
                      badgeCount :: (B.C f Kernel.Prelude.Int),
                      badgeKey :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      id :: (B.C f Kernel.Prelude.Text),
                      riderId :: (B.C f Kernel.Prelude.Text),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                      merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table FeedbackBadgeT
    where data PrimaryKey FeedbackBadgeT f = FeedbackBadgeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FeedbackBadgeId . id
type FeedbackBadge = FeedbackBadgeT Identity

$(enableKVPG (''FeedbackBadgeT) [('id)] [])

$(mkTableInstances (''FeedbackBadgeT) "feedback_badge")

