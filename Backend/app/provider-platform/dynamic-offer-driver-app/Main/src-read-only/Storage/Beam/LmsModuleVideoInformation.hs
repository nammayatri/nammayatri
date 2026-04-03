{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.LmsModuleVideoInformation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.LmsModuleVideoInformation
import qualified Database.Beam as B



data LmsModuleVideoInformationT f
    = LmsModuleVideoInformationT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                  id :: (B.C f Kernel.Prelude.Text),
                                  moduleId :: (B.C f Kernel.Prelude.Text),
                                  rank :: (B.C f Kernel.Prelude.Int),
                                  updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                                  videoStatus :: (B.C f Domain.Types.LmsModuleVideoInformation.VideoStatus)}
    deriving (Generic, B.Beamable)
instance B.Table LmsModuleVideoInformationT
    where data PrimaryKey LmsModuleVideoInformationT f = LmsModuleVideoInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = LmsModuleVideoInformationId . id
type LmsModuleVideoInformation = LmsModuleVideoInformationT Identity

$(enableKVPG (''LmsModuleVideoInformationT) [('id)] [])

$(mkTableInstances (''LmsModuleVideoInformationT) "lms_module_video_information")

