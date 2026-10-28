{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.MinimumGuarantee where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Domain.Types.MgCampaign
import qualified Domain.Types.MgCampaignRule
import qualified Domain.Types.MgEnrollment
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data MgCampaignExtendReq = MgCampaignExtendReq {endDate :: Data.Time.Day, maxBudget :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MgCampaignExtendReq where
  hideSecrets = Kernel.Prelude.identity

data MgCampaignInfo = MgCampaignInfo
  { campaignId :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign,
    name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategories :: [Dashboard.Common.VehicleCategory],
    startDate :: Data.Time.Day,
    endDate :: Data.Time.Day,
    frequency :: Domain.Types.MgCampaign.MgFrequency,
    guaranteeAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    forceAssign :: Kernel.Prelude.Bool,
    optInCutoffMinutes :: Kernel.Prelude.Int,
    settlementGraceMinutes :: Kernel.Prelude.Int,
    onFleetChange :: Domain.Types.MgCampaign.MgFleetChangePolicy,
    maxBudget :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    status :: Domain.Types.MgCampaign.MgCampaignStatus,
    createdBy :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    windows :: [MgWindow],
    rules :: [MgRule]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgCampaignListRes = MgCampaignListRes {campaigns :: [MgCampaignInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgCampaignReq = MgCampaignReq
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategories :: [Dashboard.Common.VehicleCategory],
    startDate :: Data.Time.Day,
    endDate :: Data.Time.Day,
    guaranteeAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    forceAssign :: Kernel.Prelude.Bool,
    optInCutoffMinutes :: Kernel.Prelude.Int,
    settlementGraceMinutes :: Kernel.Prelude.Int,
    onFleetChange :: Domain.Types.MgCampaign.MgFleetChangePolicy,
    maxBudget :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    windows :: [MgWindow],
    rules :: [MgRule]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MgCampaignReq where
  hideSecrets = Kernel.Prelude.identity

data MgCampaignRes = MgCampaignRes {campaignId :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgCampaignStatusReq = MgCampaignStatusReq {status :: Domain.Types.MgCampaign.MgCampaignStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MgCampaignStatusReq where
  hideSecrets = Kernel.Prelude.identity

data MgEnrollmentItem = MgEnrollmentItem
  { driverId :: Kernel.Prelude.Text,
    periodDate :: Data.Time.Day,
    status :: Domain.Types.MgEnrollment.MgEnrollmentStatus,
    beneficiaryType :: Domain.Types.MgEnrollment.MgBeneficiaryType,
    beneficiaryId :: Kernel.Prelude.Text,
    earned :: Kernel.Types.Common.HighPrecMoney,
    rides :: Kernel.Prelude.Int,
    distanceKm :: Kernel.Prelude.Int,
    breakMinutes :: Kernel.Prelude.Int,
    cancellations :: Kernel.Prelude.Int,
    payoutPercent :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    topUpAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    ledgerEntryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgEnrollmentListRes = MgEnrollmentListRes {enrollments :: [MgEnrollmentItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgInviteReq = MgInviteReq {invitees :: [MgInvitee]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MgInviteReq where
  hideSecrets = Kernel.Prelude.identity

data MgInviteRes = MgInviteRes {invited :: Kernel.Prelude.Int, skipped :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgInvitee = MgInvitee {driverId :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgRule = MgRule {metric :: Domain.Types.MgCampaignRule.MgMetric, params :: Kernel.Prelude.Maybe Domain.Types.MgCampaignRule.MgRuleParams, slabs :: [Domain.Types.MgCampaignRule.MgSlab]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MgWindow = MgWindow {startTime :: Data.Time.TimeOfDay, endTime :: Data.Time.TimeOfDay}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("minimumGuarantee" :> (GetMinimumGuaranteeList :<|> PostMinimumGuaranteeCreateHelper :<|> PostMinimumGuaranteeUpdate :<|> PostMinimumGuaranteeInvite :<|> PostMinimumGuaranteeStatus :<|> PostMinimumGuaranteeExtend :<|> GetMinimumGuaranteeEnrollments))

type GetMinimumGuaranteeList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] MgCampaignListRes)

type PostMinimumGuaranteeCreate = ("create" :> ReqBody '[JSON] MgCampaignReq :> Post '[JSON] MgCampaignRes)

type PostMinimumGuaranteeCreateHelper = ("create" :> Capture "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] MgCampaignReq :> Post '[JSON] MgCampaignRes)

type PostMinimumGuaranteeUpdate =
  ( Capture "campaignId" (Kernel.Types.Id.Id Dashboard.Common.MgCampaign) :> "update" :> ReqBody '[JSON] MgCampaignReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMinimumGuaranteeInvite = (Capture "campaignId" (Kernel.Types.Id.Id Dashboard.Common.MgCampaign) :> "invite" :> ReqBody '[JSON] MgInviteReq :> Post '[JSON] MgInviteRes)

type PostMinimumGuaranteeStatus =
  ( Capture "campaignId" (Kernel.Types.Id.Id Dashboard.Common.MgCampaign) :> "status" :> ReqBody '[JSON] MgCampaignStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMinimumGuaranteeExtend =
  ( Capture "campaignId" (Kernel.Types.Id.Id Dashboard.Common.MgCampaign) :> "extend" :> ReqBody '[JSON] MgCampaignExtendReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMinimumGuaranteeEnrollments = (Capture "campaignId" (Kernel.Types.Id.Id Dashboard.Common.MgCampaign) :> "enrollments" :> QueryParam "date" Data.Time.Day :> Get '[JSON] MgEnrollmentListRes)

data MinimumGuaranteeAPIs = MinimumGuaranteeAPIs
  { getMinimumGuaranteeList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient MgCampaignListRes,
    postMinimumGuaranteeCreate :: Kernel.Prelude.Text -> MgCampaignReq -> EulerHS.Types.EulerClient MgCampaignRes,
    postMinimumGuaranteeUpdate :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> MgCampaignReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMinimumGuaranteeInvite :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> MgInviteReq -> EulerHS.Types.EulerClient MgInviteRes,
    postMinimumGuaranteeStatus :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> MgCampaignStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMinimumGuaranteeExtend :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> MgCampaignExtendReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMinimumGuaranteeEnrollments :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> Kernel.Prelude.Maybe Data.Time.Day -> EulerHS.Types.EulerClient MgEnrollmentListRes
  }

mkMinimumGuaranteeAPIs :: (Client EulerHS.Types.EulerClient API -> MinimumGuaranteeAPIs)
mkMinimumGuaranteeAPIs minimumGuaranteeClient = (MinimumGuaranteeAPIs {..})
  where
    getMinimumGuaranteeList :<|> postMinimumGuaranteeCreate :<|> postMinimumGuaranteeUpdate :<|> postMinimumGuaranteeInvite :<|> postMinimumGuaranteeStatus :<|> postMinimumGuaranteeExtend :<|> getMinimumGuaranteeEnrollments = minimumGuaranteeClient

data MinimumGuaranteeUserActionType
  = GET_MINIMUM_GUARANTEE_LIST
  | POST_MINIMUM_GUARANTEE_CREATE
  | POST_MINIMUM_GUARANTEE_UPDATE
  | POST_MINIMUM_GUARANTEE_INVITE
  | POST_MINIMUM_GUARANTEE_STATUS
  | POST_MINIMUM_GUARANTEE_EXTEND
  | GET_MINIMUM_GUARANTEE_ENROLLMENTS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MinimumGuaranteeUserActionType])
