{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MgCampaignWindow where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.MgCampaign
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MgCampaignWindow = MgCampaignWindow
  { campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    endTime :: Data.Time.TimeOfDay,
    id :: Kernel.Types.Id.Id Domain.Types.MgCampaignWindow.MgCampaignWindow,
    startTime :: Data.Time.TimeOfDay,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
