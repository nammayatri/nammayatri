{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSFleetRating where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSFleetRatingT f = FRFSFleetRatingT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fleetNumber :: (B.C f Data.Text.Text),
    gtfsId :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    rating :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal)),
    totalRatingCount :: (B.C f Kernel.Prelude.Int),
    totalRatingScore :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSFleetRatingT where
  data PrimaryKey FRFSFleetRatingT f = FRFSFleetRatingId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSFleetRatingId . id

type FRFSFleetRating = FRFSFleetRatingT Identity

$(enableKVPG (''FRFSFleetRatingT) [('id)] [[('fleetNumber)]])

$(mkTableInstances (''FRFSFleetRatingT) "frfs_fleet_rating")
