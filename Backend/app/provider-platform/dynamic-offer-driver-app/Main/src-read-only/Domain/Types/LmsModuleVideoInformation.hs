{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LmsModuleVideoInformation where

import Data.Aeson
import qualified Domain.Types.LmsModule
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LmsModuleVideoInformation = LmsModuleVideoInformation
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    rank :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    videoStatus :: Domain.Types.LmsModuleVideoInformation.VideoStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VideoStatus = ACTIVE | INACTIVE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VideoStatus)
