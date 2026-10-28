{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MinimumGuarantee where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.MgCampaign
import qualified Domain.Types.MgCampaignRule
import qualified Domain.Types.MgEnrollment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data MgInvitation = MgInvitation
  { campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    currency :: Kernel.Types.Common.Currency,
    days :: [MgInvitationDay],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endDate :: Data.Time.Day,
    forceAssign :: Kernel.Prelude.Bool,
    guaranteeAmount :: Kernel.Types.Common.HighPrecMoney,
    name :: Kernel.Prelude.Text,
    rules :: [MgRuleRes],
    startDate :: Data.Time.Day,
    windows :: [MgWindowRes]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgInvitationDay = MgInvitationDay {canOptIn :: Kernel.Prelude.Bool, date :: Data.Time.Day, status :: Kernel.Prelude.Maybe Domain.Types.MgEnrollment.MgEnrollmentStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgInvitationsRes = MgInvitationsRes {invitations :: [MgInvitation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgOptInReq = MgOptInReq {acceptTerms :: Kernel.Prelude.Bool, campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign, date :: Data.Time.Day}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgOptOutReq = MgOptOutReq {campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign, date :: Data.Time.Day}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgProgress = MgProgress
  { breakMinutesUsed :: Kernel.Prelude.Int,
    campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    campaignName :: Kernel.Prelude.Text,
    cancellations :: Kernel.Prelude.Int,
    currency :: Kernel.Types.Common.Currency,
    date :: Data.Time.Day,
    distanceKm :: Kernel.Prelude.Int,
    earned :: Kernel.Types.Common.HighPrecMoney,
    guaranteeAmount :: Kernel.Types.Common.HighPrecMoney,
    payoutPercent :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rides :: Kernel.Prelude.Int,
    status :: Domain.Types.MgEnrollment.MgEnrollmentStatus,
    topUpAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    windows :: [MgWindowRes]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgProgressRes = MgProgressRes {days :: [MgProgress]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgRuleRes = MgRuleRes {metric :: Domain.Types.MgCampaignRule.MgMetric, slabs :: [Domain.Types.MgCampaignRule.MgSlab]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgWindowRes = MgWindowRes {endTime :: Data.Time.TimeOfDay, startTime :: Data.Time.TimeOfDay}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
