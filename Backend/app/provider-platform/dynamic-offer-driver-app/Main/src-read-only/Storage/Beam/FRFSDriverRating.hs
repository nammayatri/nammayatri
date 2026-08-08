{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSDriverRating where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSDriverRatingT f = FRFSDriverRatingT
  { bookingId :: (B.C f Data.Text.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Data.Text.Text),
    driverRatingValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    feedbackDetails :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    fleetNumber :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    fleetRatingValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    gtfsId :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    id :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    operatorBadgeToken :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSDriverRatingT where
  data PrimaryKey FRFSDriverRatingT f = FRFSDriverRatingId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSDriverRatingId . id

type FRFSDriverRating = FRFSDriverRatingT Identity

$(enableKVPG (''FRFSDriverRatingT) [('id)] [[('bookingId)], [('driverId)]])

$(mkTableInstances (''FRFSDriverRatingT) "frfs_driver_rating")
