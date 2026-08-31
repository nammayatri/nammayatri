{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.IncentiveJourney.Endpoints.IncentiveJourney where

import qualified Dashboard.Common
import Data.Aeson
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.TH
import Servant hiding (Summary)
import Servant.Client

data AssignUserToIncentiveJourneyReq = AssignUserToIncentiveJourneyReq
  { personId :: Kernel.Types.Id.Id Dashboard.Common.Person,
    cohortJourneyMappingId :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping,
    isTestGroup :: Kernel.Prelude.Bool,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AssignUserToIncentiveJourneyReq where
  hideSecrets = Kernel.Prelude.identity

data CohortJourneyMappingListItem = CohortJourneyMappingListItem
  { cohortJourneyMappingId :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping,
    merchantId :: Kernel.Prelude.Text,
    cohortId :: Kernel.Types.Id.Id Dashboard.Common.CohortDetails,
    cohortName :: Kernel.Prelude.Text,
    cohortCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cohortRule :: Kernel.Prelude.Maybe Data.Aeson.Value,
    journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    journeyName :: Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Bool,
    journeyType :: Kernel.Prelude.Maybe IncentiveJourneyType,
    startDate :: Kernel.Prelude.UTCTime,
    endDate :: Kernel.Prelude.UTCTime,
    streakRange :: Kernel.Prelude.Int,
    streakEndRewardType :: Kernel.Prelude.Maybe MilestoneRewardType,
    streakEndRewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndSubscriptionWaiveOff :: Kernel.Prelude.Maybe SubscriptionWaiveOffConfig,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CohortJourneyMappingListRes = CohortJourneyMappingListRes {mappings :: [CohortJourneyMappingListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateCohortDetailsReq = CreateCohortDetailsReq
  { name :: Kernel.Prelude.Text,
    category :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cohortRule :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCohortDetailsReq where
  hideSecrets = Kernel.Prelude.identity

data CreateCohortDetailsRes = CreateCohortDetailsRes {cohortId :: Kernel.Types.Id.Id Dashboard.Common.CohortDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateCohortJourneyMappingReq = CreateCohortJourneyMappingReq
  { cohortId :: Kernel.Types.Id.Id Dashboard.Common.CohortDetails,
    journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    startDate :: Kernel.Prelude.UTCTime,
    streakRange :: Kernel.Prelude.Int,
    streakEndRewardType :: Kernel.Prelude.Maybe MilestoneRewardType,
    streakEndRewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndSubscriptionWaiveOff :: Kernel.Prelude.Maybe SubscriptionWaiveOffConfig
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCohortJourneyMappingReq where
  hideSecrets = Kernel.Prelude.identity

data CreateCohortJourneyMappingRes = CreateCohortJourneyMappingRes {cohortJourneyMappingId :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateIncentiveJourneyMilestoneReq = CreateIncentiveJourneyMilestoneReq
  { journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    conditionType :: MilestoneConditionType,
    conditionOperator :: MilestoneConditionOperator,
    conditionValue :: Kernel.Prelude.Int,
    areaType :: Kernel.Prelude.Maybe MilestoneAreaType,
    specialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    rewardType :: MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    subscriptionWaiveOff :: Kernel.Prelude.Maybe SubscriptionWaiveOffConfig,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateIncentiveJourneyMilestoneReq where
  hideSecrets = Kernel.Prelude.identity

data CreateIncentiveJourneyMilestoneRes = CreateIncentiveJourneyMilestoneRes {milestoneId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourneyMilestone}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateIncentiveJourneyReq = CreateIncentiveJourneyReq
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyType :: IncentiveJourneyType,
    enabled :: Kernel.Prelude.Bool,
    maxWaiveOffCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateIncentiveJourneyReq where
  hideSecrets = Kernel.Prelude.identity

data CreateIncentiveJourneyRes = CreateIncentiveJourneyRes {journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListItem = IncentiveJourneyListItem
  { journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyType :: Kernel.Prelude.Maybe IncentiveJourneyType,
    enabled :: Kernel.Prelude.Bool,
    maxWaiveOffCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListRes = IncentiveJourneyListRes {journeys :: [IncentiveJourneyListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyMilestoneListItem = IncentiveJourneyMilestoneListItem
  { milestoneId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourneyMilestone,
    journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    conditionType :: MilestoneConditionType,
    conditionOperator :: MilestoneConditionOperator,
    conditionValue :: Kernel.Prelude.Int,
    areaType :: Kernel.Prelude.Maybe MilestoneAreaType,
    specialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    rewardType :: MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    subscriptionWaiveOff :: Kernel.Prelude.Maybe SubscriptionWaiveOffConfig,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyMilestoneListRes = IncentiveJourneyMilestoneListRes {milestones :: [IncentiveJourneyMilestoneListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyPersonAssignmentItem = IncentiveJourneyPersonAssignmentItem
  { cohortId :: Kernel.Types.Id.Id Dashboard.Common.CohortDetails,
    cohortName :: Kernel.Prelude.Text,
    cohortJourneyMappingId :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping,
    journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    journeyName :: Kernel.Prelude.Text,
    journeyType :: Kernel.Prelude.Maybe IncentiveJourneyType,
    enabled :: Kernel.Prelude.Bool,
    startDate :: Kernel.Prelude.UTCTime,
    endDate :: Kernel.Prelude.UTCTime,
    streakRange :: Kernel.Prelude.Int,
    isTestGroup :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    assignedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyPersonAssignmentListRes = IncentiveJourneyPersonAssignmentListRes {assignments :: [IncentiveJourneyPersonAssignmentItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyStatsHistoryItem = IncentiveJourneyStatsHistoryItem
  { statsId :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Dashboard.Common.Person,
    journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    milestoneId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourneyMilestone,
    periodKey :: Kernel.Prelude.Text,
    conditionType :: MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    currentValue :: Kernel.Prelude.Int,
    status :: JourneyMilestoneStatus,
    rewardType :: MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyStatsHistoryRes = IncentiveJourneyStatsHistoryRes {stats :: [IncentiveJourneyStatsHistoryItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyType
  = Daily
  | Weekly
  | Monthly
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data JourneyMilestoneStatus
  = NotStarted
  | InProgress
  | Completed
  | Rewarded
  | WaivedOff
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneAreaType
  = Default
  | Pickup
  | Drop
  | PickupDrop
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneConditionOperator
  = GTE
  | GT
  | EQ
  | LTE
  | LT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneConditionType
  = RideCompleted
  | Earnings
  | Distance
  | RideDuration
  | BookingTicket
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneRewardType
  = Coins
  | Cash
  | Coupons
  | WalletMoney
  | SubscriptionWaiveOff
  | PoolingPriority
  | NoReward
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionWaiveOffConfig = SubscriptionWaiveOffConfig {percentage :: Kernel.Prelude.Int, daysValidFor :: Kernel.Prelude.Int, serviceName :: Kernel.Prelude.Text, waiveOffMode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UnassignUserFromIncentiveJourneyReq = UnassignUserFromIncentiveJourneyReq {personId :: Kernel.Types.Id.Id Dashboard.Common.Person, cohortJourneyMappingId :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UnassignUserFromIncentiveJourneyReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateCohortJourneyMappingReq = UpdateCohortJourneyMappingReq
  { cohortJourneyMappingId :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    streakRange :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardType :: Kernel.Prelude.Maybe MilestoneRewardType,
    streakEndRewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndSubscriptionWaiveOff :: Kernel.Prelude.Maybe SubscriptionWaiveOffConfig
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateCohortJourneyMappingReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateIncentiveJourneyMilestoneReq = UpdateIncentiveJourneyMilestoneReq
  { milestoneId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourneyMilestone,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    conditionType :: Kernel.Prelude.Maybe MilestoneConditionType,
    conditionOperator :: Kernel.Prelude.Maybe MilestoneConditionOperator,
    conditionValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    areaType :: Kernel.Prelude.Maybe MilestoneAreaType,
    specialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    rewardType :: Kernel.Prelude.Maybe MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    subscriptionWaiveOff :: Kernel.Prelude.Maybe SubscriptionWaiveOffConfig,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateIncentiveJourneyMilestoneReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateIncentiveJourneyReq = UpdateIncentiveJourneyReq
  { journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyType :: Kernel.Prelude.Maybe IncentiveJourneyType,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxWaiveOffCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateIncentiveJourneyReq where
  hideSecrets = Kernel.Prelude.identity

data WaiveIncentiveJourneyMilestoneReq = WaiveIncentiveJourneyMilestoneReq
  { personId :: Kernel.Types.Id.Id Dashboard.Common.Person,
    journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    milestoneId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourneyMilestone,
    periodKey :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets WaiveIncentiveJourneyMilestoneReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> GetIncentiveJourneyPersonAssignments :<|> PostIncentiveJourneyCohortCreate :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> DeleteIncentiveJourneyCohortJourney :<|> GetIncentiveJourneyCohortJourneyList :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign))

type GetIncentiveJourneyList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "enabled" Kernel.Prelude.Bool
      :> QueryParam
           "journeyId"
           (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney)
      :> QueryParam "journeyType" IncentiveJourneyType
      :> Get
           '[JSON]
           IncentiveJourneyListRes
  )

type PostIncentiveJourneyCreate = ("create" :> ReqBody '[JSON] CreateIncentiveJourneyReq :> Post '[JSON] CreateIncentiveJourneyRes)

type PutIncentiveJourneyUpdate = ("update" :> ReqBody '[JSON] UpdateIncentiveJourneyReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetIncentiveJourneyMilestoneList =
  ( "milestone" :> Capture "journeyId" (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get '[JSON] IncentiveJourneyMilestoneListRes
  )

type PostIncentiveJourneyMilestoneCreate = ("milestone" :> "create" :> ReqBody '[JSON] CreateIncentiveJourneyMilestoneReq :> Post '[JSON] CreateIncentiveJourneyMilestoneRes)

type PutIncentiveJourneyMilestoneUpdate = ("milestone" :> "update" :> ReqBody '[JSON] UpdateIncentiveJourneyMilestoneReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetIncentiveJourneyStatsHistory =
  ( "stats" :> "history" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Person)
      :> QueryParam
           "journeyId"
           (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney)
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "fromDate"
           Data.Time.Day
      :> MandatoryQueryParam
           "toDate"
           Data.Time.Day
      :> Get
           '[JSON]
           IncentiveJourneyStatsHistoryRes
  )

type PostIncentiveJourneyStatsWaiveOff = ("stats" :> "waiveOff" :> ReqBody '[JSON] WaiveIncentiveJourneyMilestoneReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetIncentiveJourneyPersonAssignments = ("person" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Person) :> "assignments" :> Get '[JSON] IncentiveJourneyPersonAssignmentListRes)

type PostIncentiveJourneyCohortCreate = ("cohort" :> "create" :> ReqBody '[JSON] CreateCohortDetailsReq :> Post '[JSON] CreateCohortDetailsRes)

type PostIncentiveJourneyCohortJourneyCreate = ("cohortJourney" :> "create" :> ReqBody '[JSON] CreateCohortJourneyMappingReq :> Post '[JSON] CreateCohortJourneyMappingRes)

type PutIncentiveJourneyCohortJourneyUpdate = ("cohortJourney" :> "update" :> ReqBody '[JSON] UpdateCohortJourneyMappingReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteIncentiveJourneyCohortJourney =
  ( "cohortJourney" :> Capture "cohortJourneyMappingId" (Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping)
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetIncentiveJourneyCohortJourneyList =
  ( "cohortJourney" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "cohortName"
           Kernel.Prelude.Text
      :> QueryParam "cohortCategory" Kernel.Prelude.Text
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "journeyType"
           IncentiveJourneyType
      :> Get
           '[JSON]
           CohortJourneyMappingListRes
  )

type PostIncentiveJourneyAssign = ("assign" :> ReqBody '[JSON] AssignUserToIncentiveJourneyReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteIncentiveJourneyUnassign = ("unassign" :> ReqBody '[JSON] UnassignUserFromIncentiveJourneyReq :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

data IncentiveJourneyAPIs = IncentiveJourneyAPIs
  { getIncentiveJourneyList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe IncentiveJourneyType -> EulerHS.Types.EulerClient IncentiveJourneyListRes,
    postIncentiveJourneyCreate :: CreateIncentiveJourneyReq -> EulerHS.Types.EulerClient CreateIncentiveJourneyRes,
    putIncentiveJourneyUpdate :: UpdateIncentiveJourneyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIncentiveJourneyMilestoneList :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient IncentiveJourneyMilestoneListRes,
    postIncentiveJourneyMilestoneCreate :: CreateIncentiveJourneyMilestoneReq -> EulerHS.Types.EulerClient CreateIncentiveJourneyMilestoneRes,
    putIncentiveJourneyMilestoneUpdate :: UpdateIncentiveJourneyMilestoneReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIncentiveJourneyStatsHistory :: Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> EulerHS.Types.EulerClient IncentiveJourneyStatsHistoryRes,
    postIncentiveJourneyStatsWaiveOff :: WaiveIncentiveJourneyMilestoneReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIncentiveJourneyPersonAssignments :: Kernel.Types.Id.Id Dashboard.Common.Person -> EulerHS.Types.EulerClient IncentiveJourneyPersonAssignmentListRes,
    postIncentiveJourneyCohortCreate :: CreateCohortDetailsReq -> EulerHS.Types.EulerClient CreateCohortDetailsRes,
    postIncentiveJourneyCohortJourneyCreate :: CreateCohortJourneyMappingReq -> EulerHS.Types.EulerClient CreateCohortJourneyMappingRes,
    putIncentiveJourneyCohortJourneyUpdate :: UpdateCohortJourneyMappingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteIncentiveJourneyCohortJourney :: Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIncentiveJourneyCohortJourneyList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe IncentiveJourneyType -> EulerHS.Types.EulerClient CohortJourneyMappingListRes,
    postIncentiveJourneyAssign :: AssignUserToIncentiveJourneyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteIncentiveJourneyUnassign :: UnassignUserFromIncentiveJourneyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkIncentiveJourneyAPIs :: (Client EulerHS.Types.EulerClient API -> IncentiveJourneyAPIs)
mkIncentiveJourneyAPIs incentiveJourneyClient = (IncentiveJourneyAPIs {..})
  where
    getIncentiveJourneyList :<|> postIncentiveJourneyCreate :<|> putIncentiveJourneyUpdate :<|> getIncentiveJourneyMilestoneList :<|> postIncentiveJourneyMilestoneCreate :<|> putIncentiveJourneyMilestoneUpdate :<|> getIncentiveJourneyStatsHistory :<|> postIncentiveJourneyStatsWaiveOff :<|> getIncentiveJourneyPersonAssignments :<|> postIncentiveJourneyCohortCreate :<|> postIncentiveJourneyCohortJourneyCreate :<|> putIncentiveJourneyCohortJourneyUpdate :<|> deleteIncentiveJourneyCohortJourney :<|> getIncentiveJourneyCohortJourneyList :<|> postIncentiveJourneyAssign :<|> deleteIncentiveJourneyUnassign = incentiveJourneyClient

data IncentiveJourneyUserActionType
  = GET_INCENTIVE_JOURNEY_LIST
  | POST_INCENTIVE_JOURNEY_CREATE
  | PUT_INCENTIVE_JOURNEY_UPDATE
  | GET_INCENTIVE_JOURNEY_MILESTONE_LIST
  | POST_INCENTIVE_JOURNEY_MILESTONE_CREATE
  | PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE
  | GET_INCENTIVE_JOURNEY_STATS_HISTORY
  | POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF
  | GET_INCENTIVE_JOURNEY_PERSON_ASSIGNMENTS
  | POST_INCENTIVE_JOURNEY_COHORT_CREATE
  | POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE
  | PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE
  | DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY
  | GET_INCENTIVE_JOURNEY_COHORT_JOURNEY_LIST
  | POST_INCENTIVE_JOURNEY_ASSIGN
  | DELETE_INCENTIVE_JOURNEY_UNASSIGN
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''IncentiveJourneyType)

$(Data.Singletons.TH.genSingletons [''IncentiveJourneyUserActionType])
