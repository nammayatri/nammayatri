{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.ShuttleInterest where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified "this" Domain.Types.PassOrganization
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.ShuttleInterest
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ShuttleInterestItem = ShuttleInterestItem
  { companyDomain :: Kernel.Prelude.Text,
    companyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Data.Time.UTCTime,
    daysOfWeek :: [Data.Time.Calendar.DayOfWeek],
    dropAddress :: Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Double,
    dropLon :: Kernel.Prelude.Double,
    dropTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    employeeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pickupAddress :: Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    pickupTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType,
    updatedAt :: Data.Time.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleInterestListResp = ShuttleInterestListResp {interests :: [ShuttleInterestItem], limit :: Kernel.Prelude.Int, offset :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleInterestMapPoint = ShuttleInterestMapPoint
  { companyDomain :: Kernel.Prelude.Text,
    companyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropAddress :: Kernel.Prelude.Text,
    dropLat :: Kernel.Prelude.Double,
    dropLon :: Kernel.Prelude.Double,
    employeeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pickupAddress :: Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleInterestMapResp = ShuttleInterestMapResp {points :: [ShuttleInterestMapPoint]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttlePublishedRoute = ShuttlePublishedRoute
  { distanceMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    durationMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    fareAmount :: Kernel.Types.Common.HighPrecMoney,
    firstDeparture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    routeCode :: Kernel.Prelude.Text,
    routeType :: Kernel.Prelude.Text,
    shape :: [[Kernel.Prelude.Double]],
    shortName :: Kernel.Prelude.Text,
    stops :: [ShuttlePublishedStop],
    tripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    trips :: [ShuttlePublishedTripInfo]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttlePublishedRoutesResp = ShuttlePublishedRoutesResp {feedKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text, routes :: [ShuttlePublishedRoute]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttlePublishedStop = ShuttlePublishedStop
  { lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    minutesFromStart :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Text,
    sequence :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttlePublishedTripInfo = ShuttlePublishedTripInfo
  { days :: [Kernel.Prelude.Text],
    departure :: Kernel.Prelude.Text,
    direction :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riders :: Kernel.Prelude.Int,
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleRoutesPublishResp = ShuttleRoutesPublishResp {fileName :: Kernel.Prelude.Text, gtfsZipBase64 :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleSubscriptionItem = ShuttleSubscriptionItem
  { amount :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    attendance :: Kernel.Prelude.Text,
    companyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    daysOfWeek :: [Data.Time.Calendar.DayOfWeek],
    destinationStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    employeeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pickupAddress :: Kernel.Prelude.Text,
    pickupTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    routeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripType :: Domain.Types.ShuttleInterest.ShuttleTripType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShuttleSubscriptionListResp = ShuttleSubscriptionListResp {subscriptions :: [ShuttleSubscriptionItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = "shuttleInterest" :> (GetShuttleInterestList :<|> (GetShuttleInterestMapPoints :<|> (GetShuttleInterestPublishedRoutes :<|> (PostShuttleInterestShuttleRoutesPublish :<|> GetShuttleInterestSubscriptions))))

type GetShuttleInterestList =
  "list"
    :> ( QueryParam "passOrganizationId" (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization)
           :> ( QueryParam
                  "companyDomain"
                  Kernel.Prelude.Text
                  :> ( QueryParam "limit" Kernel.Prelude.Int
                         :> ( QueryParam "offset" Kernel.Prelude.Int
                                :> Get
                                     ('[JSON])
                                     ShuttleInterestListResp
                            )
                     )
              )
       )

type GetShuttleInterestMapPoints =
  "mapPoints"
    :> ( QueryParam "passOrganizationId" (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization)
           :> ( QueryParam
                  "companyDomain"
                  Kernel.Prelude.Text
                  :> Get ('[JSON]) ShuttleInterestMapResp
              )
       )

type GetShuttleInterestPublishedRoutes = "publishedRoutes" :> (QueryParam "day" Kernel.Prelude.Text :> (QueryParam "subscribedOnly" Kernel.Prelude.Bool :> Get ('[JSON]) ShuttlePublishedRoutesResp))

type PostShuttleInterestShuttleRoutesPublish = "shuttleRoutes" :> ("publish" :> (QueryParam "subscribedOnly" Kernel.Prelude.Bool :> Post ('[JSON]) ShuttleRoutesPublishResp))

type GetShuttleInterestSubscriptions = "subscriptions" :> Get ('[JSON]) ShuttleSubscriptionListResp

data ShuttleInterestAPIs = ShuttleInterestAPIs
  { getShuttleInterestList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient ShuttleInterestListResp)))),
    getShuttleInterestMapPoints :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient ShuttleInterestMapResp)),
    getShuttleInterestPublishedRoutes :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> EulerHS.Types.EulerClient ShuttlePublishedRoutesResp)),
    postShuttleInterestShuttleRoutesPublish :: (Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> EulerHS.Types.EulerClient ShuttleRoutesPublishResp),
    getShuttleInterestSubscriptions :: (EulerHS.Types.EulerClient ShuttleSubscriptionListResp)
  }

mkShuttleInterestAPIs :: Client EulerHS.Types.EulerClient API -> ShuttleInterestAPIs
mkShuttleInterestAPIs shuttleInterestClient = (ShuttleInterestAPIs {..})
  where
    getShuttleInterestList :<|> getShuttleInterestMapPoints :<|> getShuttleInterestPublishedRoutes :<|> postShuttleInterestShuttleRoutesPublish :<|> getShuttleInterestSubscriptions = shuttleInterestClient

data ShuttleInterestUserActionType
  = GET_SHUTTLE_INTEREST_LIST
  | GET_SHUTTLE_INTEREST_MAP_POINTS
  | GET_SHUTTLE_INTEREST_PUBLISHED_ROUTES
  | POST_SHUTTLE_INTEREST_SHUTTLE_ROUTES_PUBLISH
  | GET_SHUTTLE_INTEREST_SUBSCRIPTIONS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''ShuttleInterestUserActionType)])
