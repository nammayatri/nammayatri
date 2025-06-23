{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.EventManagement where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.DraftTicketChange
import qualified "this" Domain.Types.EventManagement
import qualified "this" Domain.Types.MerchantOnboarding
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data RecommendToggleReq = RecommendToggleReq {placeId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace, recommend :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReviewDraftReq = ReviewDraftReq {message :: Kernel.Prelude.Maybe Kernel.Prelude.Text, status :: ReviewStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReviewStatus
  = APPROVE
  | REJECT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

type API = (GetEventManagementTicketdashboardTicketplaceDef :<|> PostEventManagementTicketdashboardTicketplaceCleardraft :<|> PostEventManagementTicketdashboardTicketplaceSubmitDraft :<|> PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft :<|> PostEventManagementTicketdashboardTicketplaceReviewDraft :<|> PostEventManagementTicketdashboardTicketplaceCreate :<|> PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo :<|> PostEventManagementTicketdashboardTicketplaceUpdateService :<|> PostEventManagementTicketdashboardTicketplaceDelService :<|> PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory :<|> PostEventManagementTicketdashboardTicketplaceServiceDelCategory :<|> PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :<|> PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople :<|> GetEventManagementTicketdashboardTicketplaceDrafts :<|> PostEventManagementTicketdashboardTicketplaceRecommend)

type GetEventManagementTicketdashboardTicketplaceDef =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "def"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketplaceCleardraft =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "cleardraft"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostEventManagementTicketdashboardTicketplaceSubmitDraft =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "submitDraft"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "cancelSubmitDraft"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostEventManagementTicketdashboardTicketplaceReviewDraft =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "reviewDraft"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           '[JSON]
           ReviewDraftReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostEventManagementTicketdashboardTicketplaceCreate =
  ( "ticketdashboard" :> "ticketplace" :> "create" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] Domain.Types.EventManagement.BasicInformation
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "update"
      :> "basicInfo"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           '[JSON]
           Domain.Types.EventManagement.BasicInformation
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketplaceUpdateService =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "updateService"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           '[JSON]
           Domain.Types.EventManagement.TicketServiceDef
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketplaceDelService =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "delService"
      :> Capture "serviceId" (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "service"
      :> Capture
           "serviceId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> "updateCategory"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           '[JSON]
           Domain.Types.EventManagement.ServiceCategoryDef
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketplaceServiceDelCategory =
  ( "ticketdashboard" :> "ticketplace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "service"
      :> Capture
           "serviceId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> "delCategory"
      :> Capture
           "categoryId"
           (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory)
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople =
  ( "ticketdashboard" :> "ticketPlace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "category"
      :> Capture
           "categoryId"
           (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory)
      :> "updatePeople"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody
           '[JSON]
           Domain.Types.EventManagement.ServicePeopleCategoryDef
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople =
  ( "ticketdashboard" :> "ticketPlace"
      :> Capture
           "ticketPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "category"
      :> Capture
           "categoryId"
           (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory)
      :> "delPeople"
      :> Capture
           "peopleId"
           (Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory)
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Post
           '[JSON]
           Domain.Types.EventManagement.TicketPlaceDef
  )

type GetEventManagementTicketdashboardTicketplaceDrafts =
  ( "ticketdashboard" :> "ticketplace" :> "drafts" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> MandatoryQueryParam "limit" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "status"
           Domain.Types.DraftTicketChange.DraftStatus
      :> Get
           '[JSON]
           [Domain.Types.EventManagement.TicketPlaceDef]
  )

type PostEventManagementTicketdashboardTicketplaceRecommend =
  ( "ticketdashboard" :> "ticketplace" :> "recommend" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] [RecommendToggleReq]
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data EventManagementAPIs = EventManagementAPIs
  { getEventManagementTicketdashboardTicketplaceDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketplaceCleardraft :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postEventManagementTicketdashboardTicketplaceSubmitDraft :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postEventManagementTicketdashboardTicketplaceReviewDraft :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> ReviewDraftReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postEventManagementTicketdashboardTicketplaceCreate :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.EventManagement.BasicInformation -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.EventManagement.BasicInformation -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketplaceUpdateService :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.EventManagement.TicketServiceDef -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketplaceDelService :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.EventManagement.ServiceCategoryDef -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketplaceServiceDelCategory :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.EventManagement.TicketPlaceDef,
    getEventManagementTicketdashboardTicketplaceDrafts :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Domain.Types.DraftTicketChange.DraftStatus -> EulerHS.Types.EulerClient [Domain.Types.EventManagement.TicketPlaceDef],
    postEventManagementTicketdashboardTicketplaceRecommend :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> [RecommendToggleReq] -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkEventManagementAPIs :: (Client EulerHS.Types.EulerClient API -> EventManagementAPIs)
mkEventManagementAPIs eventManagementClient = (EventManagementAPIs {..})
  where
    getEventManagementTicketdashboardTicketplaceDef :<|> postEventManagementTicketdashboardTicketplaceCleardraft :<|> postEventManagementTicketdashboardTicketplaceSubmitDraft :<|> postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :<|> postEventManagementTicketdashboardTicketplaceReviewDraft :<|> postEventManagementTicketdashboardTicketplaceCreate :<|> postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :<|> postEventManagementTicketdashboardTicketplaceUpdateService :<|> postEventManagementTicketdashboardTicketplaceDelService :<|> postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :<|> postEventManagementTicketdashboardTicketplaceServiceDelCategory :<|> postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :<|> postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :<|> getEventManagementTicketdashboardTicketplaceDrafts :<|> postEventManagementTicketdashboardTicketplaceRecommend = eventManagementClient

data EventManagementUserActionType
  = GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEF
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CLEARDRAFT
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SUBMIT_DRAFT
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CANCEL_SUBMIT_DRAFT
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_REVIEW_DRAFT
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CREATE
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_BASIC_INFO
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_SERVICE
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEL_SERVICE
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_UPDATE_CATEGORY
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_DEL_CATEGORY
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_UPDATE_PEOPLE
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_DEL_PEOPLE
  | GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DRAFTS
  | POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_RECOMMEND
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''ReviewStatus)

$(Data.Singletons.TH.genSingletons [''EventManagementUserActionType])
