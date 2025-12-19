{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Feedback where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FeedbackT f = FeedbackT
  { badge :: B.C f Kernel.Prelude.Text,
    badgeKey :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    rating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    rideId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackT where
  data PrimaryKey FeedbackT f = FeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FeedbackId . id

type Feedback = FeedbackT Identity

$(enableKVPG ''FeedbackT ['id] [])

$(mkTableInstances ''FeedbackT "feedback")
