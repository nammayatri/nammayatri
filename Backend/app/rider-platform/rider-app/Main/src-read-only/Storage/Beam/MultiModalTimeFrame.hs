{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalTimeFrame where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MultiModalTimeFrameT f = MultiModalTimeFrameT
  { endTime :: (B.C f Kernel.Prelude.TimeOfDay),
    id :: (B.C f Kernel.Prelude.Text),
    serviceId :: (B.C f Kernel.Prelude.Text),
    startTime :: (B.C f Kernel.Prelude.TimeOfDay),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalTimeFrameT where
  data PrimaryKey MultiModalTimeFrameT f = MultiModalTimeFrameId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalTimeFrameId . id

type MultiModalTimeFrame = MultiModalTimeFrameT Identity

$(enableKVPG (''MultiModalTimeFrameT) [('id)] [])

$(mkTableInstances (''MultiModalTimeFrameT) "multi_modal_time_frame")
