{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DetectionSource where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DetectionSource
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DetectionSourceT f = DetectionSourceT
  { anomalyAlertsEnabled :: (B.C f Kernel.Prelude.Bool),
    authCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    authMode :: (B.C f Domain.Types.DetectionSource.DetectionAuthMode),
    authSecret :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    enforcementEnabled :: (B.C f Kernel.Prelude.Bool),
    enforcementGraceMinutes :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    imagesEnabled :: (B.C f Kernel.Prelude.Bool),
    mappings :: (B.C f Data.Aeson.Value),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    occupancyMode :: (B.C f Domain.Types.DetectionSource.OccupancyMode),
    vendorKey :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DetectionSourceT where
  data PrimaryKey DetectionSourceT f = DetectionSourceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DetectionSourceId . id

type DetectionSource = DetectionSourceT Identity

$(enableKVPG (''DetectionSourceT) [('id)] [])

$(mkTableInstances (''DetectionSourceT) "detection_source")
