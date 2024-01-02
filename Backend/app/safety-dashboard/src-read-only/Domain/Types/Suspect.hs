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

data FlaggedBy = FlaggedBy {flaggedCategory :: Kernel.Prelude.Text, partnerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read)

data FlaggedStatus = Flagged | Charged | Clean deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FlaggedBy))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FlaggedStatus))

$(mkHttpInstancesForEnum (''FlaggedStatus))
