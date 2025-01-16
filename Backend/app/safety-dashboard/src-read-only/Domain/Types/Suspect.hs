{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Suspect where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Suspect = Suspect
  { createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    flagUpdatedAt :: Kernel.Prelude.UTCTime,
    flaggedBy :: [Domain.Types.Suspect.FlaggedBy],
    flaggedCounter :: Kernel.Prelude.Int,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Kernel.Types.Id.Id Domain.Types.Suspect.Suspect,
    lastName :: Kernel.Prelude.Text,
    statusChangedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FlaggedBy = FlaggedBy
  { agentContactNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    flaggedCategory :: Kernel.Prelude.Text,
    flaggedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    partnerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reportDetails :: Kernel.Prelude.Maybe Domain.Types.Suspect.ReportDetails,
    totalComplaintsCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read)

data FlaggedStatus = Flagged | Confirmed | NotConfirmed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReportDetails = ReportDetails {reportNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text, reportPoliceStation :: Kernel.Prelude.Maybe Kernel.Prelude.Text, reportType :: Domain.Types.Suspect.ReportType}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read)

data ReportType = FIR | NCR deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FlaggedBy)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FlaggedStatus)

$(mkHttpInstancesForEnum ''FlaggedStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReportDetails)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReportType)

$(mkHttpInstancesForEnum ''ReportType)
