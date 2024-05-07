{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
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

data FIRDetails = FIRDetails {firNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text, policeStation :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read)

data FlaggedBy = FlaggedBy
  { agentContactNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firDetails :: Kernel.Prelude.Maybe Domain.Types.Suspect.FIRDetails,
    flaggedCategory :: Kernel.Prelude.Text,
    flaggedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    partnerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalComplaintsCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read)

data FlaggedStatus = Flagged | Confirmed | NotConfirmed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FIRDetails))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FlaggedBy))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FlaggedStatus))

$(mkHttpInstancesForEnum (''FlaggedStatus))
