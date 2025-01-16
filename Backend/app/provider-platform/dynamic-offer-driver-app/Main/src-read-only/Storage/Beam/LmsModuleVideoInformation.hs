{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LmsModuleVideoInformation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.LmsModuleVideoInformation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data LmsModuleVideoInformationT f = LmsModuleVideoInformationT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    moduleId :: B.C f Kernel.Prelude.Text,
    rank :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    videoStatus :: B.C f Domain.Types.LmsModuleVideoInformation.VideoStatus
  }
  deriving (Generic, B.Beamable)

instance B.Table LmsModuleVideoInformationT where
  data PrimaryKey LmsModuleVideoInformationT f = LmsModuleVideoInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LmsModuleVideoInformationId . id

type LmsModuleVideoInformation = LmsModuleVideoInformationT Identity

$(enableKVPG ''LmsModuleVideoInformationT ['id] [])

$(mkTableInstances ''LmsModuleVideoInformationT "lms_module_video_information")
