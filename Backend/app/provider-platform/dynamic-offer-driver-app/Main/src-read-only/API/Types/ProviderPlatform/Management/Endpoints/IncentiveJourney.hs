{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.IncentiveJourney where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Servant
import Servant.Client

data CreateIncentiveJourneyMilestoneReq = CreateIncentiveJourneyMilestoneReq
  { journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    conditionType :: MilestoneConditionType,
    conditionOperator :: MilestoneConditionOperator,
    conditionValue :: Kernel.Prelude.Int,
    pickupSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    dropSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    rewardType :: MilestoneRewardType,
    rewardConfigId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CoinsConfig),
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int
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
    driverTag :: Kernel.Prelude.Text,
    journeyType :: IncentiveJourneyType,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    startDate :: Kernel.Prelude.UTCTime,
    endDate :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant,
    enabled :: Kernel.Prelude.Bool
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
    driverTag :: Kernel.Prelude.Text,
    journeyType :: Kernel.Prelude.Maybe IncentiveJourneyType,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    startDate :: Kernel.Prelude.UTCTime,
    endDate :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant,
    enabled :: Kernel.Prelude.Bool,
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
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    conditionType :: MilestoneConditionType,
    conditionOperator :: MilestoneConditionOperator,
    conditionValue :: Kernel.Prelude.Int,
    pickupSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    dropSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    rewardType :: MilestoneRewardType,
    rewardConfigId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CoinsConfig),
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyMilestoneListRes = IncentiveJourneyMilestoneListRes {milestones :: [IncentiveJourneyMilestoneListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyType
  = Daily
  | Weekly
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneConditionOperator
  = GTE
  | GT
  | EQ
  | LTE
  | LT
  | CT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneConditionType
  = RideCompleted
  | Earnings
  | Distance
  | RideDuration
  | PickupSpecialLocation
  | DropSpecialLocation
  | PickupDropSpecialLocation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MilestoneRewardType
  = Coins
  | Cash
  | Coupons
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateIncentiveJourneyMilestoneReq = UpdateIncentiveJourneyMilestoneReq
  { milestoneId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourneyMilestone,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    conditionType :: Kernel.Prelude.Maybe MilestoneConditionType,
    conditionOperator :: Kernel.Prelude.Maybe MilestoneConditionOperator,
    conditionValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pickupSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    dropSpecialLocationIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    rewardType :: Kernel.Prelude.Maybe MilestoneRewardType,
    rewardConfigId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CoinsConfig),
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateIncentiveJourneyMilestoneReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateIncentiveJourneyReq = UpdateIncentiveJourneyReq
  { journeyId :: Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyType :: Kernel.Prelude.Maybe IncentiveJourneyType,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    endDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateIncentiveJourneyReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate))

type GetIncentiveJourneyList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "enabled" Kernel.Prelude.Bool
      :> QueryParam
           "driverTag"
           Kernel.Prelude.Text
      :> Get '[JSON] IncentiveJourneyListRes
  )

type PostIncentiveJourneyCreate = ("create" :> ReqBody '[JSON] CreateIncentiveJourneyReq :> Post '[JSON] CreateIncentiveJourneyRes)

type PutIncentiveJourneyUpdate = ("update" :> ReqBody '[JSON] UpdateIncentiveJourneyReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetIncentiveJourneyMilestoneList =
  ( "milestone" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "journeyId"
           (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney)
      :> Get '[JSON] IncentiveJourneyMilestoneListRes
  )

type PostIncentiveJourneyMilestoneCreate = ("milestone" :> "create" :> ReqBody '[JSON] CreateIncentiveJourneyMilestoneReq :> Post '[JSON] CreateIncentiveJourneyMilestoneRes)

type PutIncentiveJourneyMilestoneUpdate = ("milestone" :> "update" :> ReqBody '[JSON] UpdateIncentiveJourneyMilestoneReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

data IncentiveJourneyAPIs = IncentiveJourneyAPIs
  { getIncentiveJourneyList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient IncentiveJourneyListRes,
    postIncentiveJourneyCreate :: CreateIncentiveJourneyReq -> EulerHS.Types.EulerClient CreateIncentiveJourneyRes,
    putIncentiveJourneyUpdate :: UpdateIncentiveJourneyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIncentiveJourneyMilestoneList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> EulerHS.Types.EulerClient IncentiveJourneyMilestoneListRes,
    postIncentiveJourneyMilestoneCreate :: CreateIncentiveJourneyMilestoneReq -> EulerHS.Types.EulerClient CreateIncentiveJourneyMilestoneRes,
    putIncentiveJourneyMilestoneUpdate :: UpdateIncentiveJourneyMilestoneReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkIncentiveJourneyAPIs :: (Client EulerHS.Types.EulerClient API -> IncentiveJourneyAPIs)
mkIncentiveJourneyAPIs incentiveJourneyClient = (IncentiveJourneyAPIs {..})
  where
    getIncentiveJourneyList :<|> postIncentiveJourneyCreate :<|> putIncentiveJourneyUpdate :<|> getIncentiveJourneyMilestoneList :<|> postIncentiveJourneyMilestoneCreate :<|> putIncentiveJourneyMilestoneUpdate = incentiveJourneyClient

data IncentiveJourneyUserActionType
  = GET_INCENTIVE_JOURNEY_LIST
  | POST_INCENTIVE_JOURNEY_CREATE
  | PUT_INCENTIVE_JOURNEY_UPDATE
  | GET_INCENTIVE_JOURNEY_MILESTONE_LIST
  | POST_INCENTIVE_JOURNEY_MILESTONE_CREATE
  | PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''IncentiveJourneyUserActionType])
