{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VersionStageMapping where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VersionStageMapping = VersionStageMapping
  { failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VersionStageMapping.VersionStageMapping,
    stageData :: Kernel.Prelude.Maybe Domain.Types.VersionStageMapping.StageData,
    stageId :: Kernel.Prelude.Text,
    stageName :: Kernel.Prelude.Text,
    status :: Domain.Types.VersionStageMapping.Status,
    versionId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FareDataType = FareDataType {id :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read, Eq, Ord)

data GtfsDataType = GtfsDataType {gtfsFilePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text, customFilePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read, Eq, Ord)

data StageData = GTFSData Domain.Types.VersionStageMapping.GtfsDataType | FAREData Domain.Types.VersionStageMapping.FareDataType deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Status = Inprogress | Completed | Failed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''StageData)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Status)
