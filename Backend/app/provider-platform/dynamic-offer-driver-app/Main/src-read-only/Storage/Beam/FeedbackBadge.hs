{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FeedbackBadge where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FeedbackBadgeT f = FeedbackBadgeT
  { badge :: B.C f Kernel.Prelude.Text,
    badgeCount :: B.C f Kernel.Prelude.Int,
    badgeKey :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackBadgeT where
  data PrimaryKey FeedbackBadgeT f = FeedbackBadgeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FeedbackBadgeId . id

type FeedbackBadge = FeedbackBadgeT Identity

$(enableKVPG ''FeedbackBadgeT ['id] [])

$(mkTableInstances ''FeedbackBadgeT "feedback_badge")
