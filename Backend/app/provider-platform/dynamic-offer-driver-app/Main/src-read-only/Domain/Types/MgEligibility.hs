{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MgEligibility where

import Data.Aeson
import qualified Domain.Types.MgCampaign
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MgEligibility = MgEligibility
  { campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.MgEligibility.MgEligibility,
    invitedAt :: Kernel.Prelude.UTCTime,
    notifiedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Domain.Types.MgEligibility.MgEligibilitySource,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data MgEligibilitySource = DASHBOARD_UPLOAD | RULE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgEligibilitySource)
