{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.EventManagement
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.EventManagement
import qualified Domain.Action.Dashboard.AppManagement.EventManagement
import qualified Domain.Types.DraftTicketChange
import qualified "this" Domain.Types.EventManagement
import qualified Domain.Types.Merchant
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = (GetEventManagementTicketdashboardTicketplaceDef :<|> PostEventManagementTicketdashboardTicketplaceCleardraft :<|> PostEventManagementTicketdashboardTicketplaceSubmitDraft :<|> PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft :<|> PostEventManagementTicketdashboardTicketplaceReviewDraft :<|> PostEventManagementTicketdashboardTicketplaceCreate :<|> PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo :<|> PostEventManagementTicketdashboardTicketplaceUpdateService :<|> PostEventManagementTicketdashboardTicketplaceDelService :<|> PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory :<|> PostEventManagementTicketdashboardTicketplaceServiceDelCategory :<|> PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :<|> PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople :<|> GetEventManagementTicketdashboardTicketplaceDrafts :<|> PostEventManagementTicketdashboardTicketplaceRecommend)

type GetEventManagementTicketdashboardTicketplaceDef =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEF"
      :> API.Types.Dashboard.AppManagement.EventManagement.GetEventManagementTicketdashboardTicketplaceDef
  )

type PostEventManagementTicketdashboardTicketplaceCleardraft =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CLEARDRAFT"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceCleardraft
  )

type PostEventManagementTicketdashboardTicketplaceSubmitDraft =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SUBMIT_DRAFT"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceSubmitDraft
  )

type PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CANCEL_SUBMIT_DRAFT"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft
  )

type PostEventManagementTicketdashboardTicketplaceReviewDraft =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_REVIEW_DRAFT"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceReviewDraft
  )

type PostEventManagementTicketdashboardTicketplaceCreate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CREATE"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceCreate
  )

type PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_BASIC_INFO"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo
  )

type PostEventManagementTicketdashboardTicketplaceUpdateService =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_SERVICE"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceUpdateService
  )

type PostEventManagementTicketdashboardTicketplaceDelService =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEL_SERVICE"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceDelService
  )

type PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_UPDATE_CATEGORY"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory
  )

type PostEventManagementTicketdashboardTicketplaceServiceDelCategory =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_DEL_CATEGORY"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceServiceDelCategory
  )

type PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_UPDATE_PEOPLE"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople
  )

type PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_DEL_PEOPLE"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople
  )

type GetEventManagementTicketdashboardTicketplaceDrafts =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DRAFTS"
      :> API.Types.Dashboard.AppManagement.EventManagement.GetEventManagementTicketdashboardTicketplaceDrafts
  )

type PostEventManagementTicketdashboardTicketplaceRecommend =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_RECOMMEND"
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceRecommend
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getEventManagementTicketdashboardTicketplaceDef merchantId city :<|> postEventManagementTicketdashboardTicketplaceCleardraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceSubmitDraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceCancelSubmitDraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceReviewDraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceCreate merchantId city :<|> postEventManagementTicketdashboardTicketplaceUpdateBasicInfo merchantId city :<|> postEventManagementTicketdashboardTicketplaceUpdateService merchantId city :<|> postEventManagementTicketdashboardTicketplaceDelService merchantId city :<|> postEventManagementTicketdashboardTicketplaceServiceUpdateCategory merchantId city :<|> postEventManagementTicketdashboardTicketplaceServiceDelCategory merchantId city :<|> postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople merchantId city :<|> postEventManagementTicketdashboardTicketPlaceCategoryDelPeople merchantId city :<|> getEventManagementTicketdashboardTicketplaceDrafts merchantId city :<|> postEventManagementTicketdashboardTicketplaceRecommend merchantId city

getEventManagementTicketdashboardTicketplaceDef :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
getEventManagementTicketdashboardTicketplaceDef a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.getEventManagementTicketdashboardTicketplaceDef a4 a3 a1

postEventManagementTicketdashboardTicketplaceCleardraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCleardraft a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceCleardraft a4 a3 a1

postEventManagementTicketdashboardTicketplaceSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceSubmitDraft a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceSubmitDraft a4 a3 a1

postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCancelSubmitDraft a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceCancelSubmitDraft a4 a3 a1

postEventManagementTicketdashboardTicketplaceReviewDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.EventManagement.ReviewDraftReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceReviewDraft a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceReviewDraft a5 a4 a2 a1

postEventManagementTicketdashboardTicketplaceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Domain.Types.EventManagement.BasicInformation -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceCreate a4 a3 a1

postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.BasicInformation -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateBasicInfo a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceUpdateBasicInfo a5 a4 a2 a1

postEventManagementTicketdashboardTicketplaceUpdateService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.TicketServiceDef -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateService a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceUpdateService a5 a4 a2 a1

postEventManagementTicketdashboardTicketplaceDelService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceDelService a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceDelService a5 a4 a2 a1

postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceUpdateCategory a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceServiceUpdateCategory a6 a5 a3 a2 a1

postEventManagementTicketdashboardTicketplaceServiceDelCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceDelCategory a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceServiceDelCategory a6 a5 a3 a2 a1

postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople a6 a5 a3 a2 a1

postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryDelPeople a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketPlaceCategoryDelPeople a6 a5 a3 a2 a1

getEventManagementTicketdashboardTicketplaceDrafts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Domain.Types.DraftTicketChange.DraftStatus -> Environment.FlowHandler [Domain.Types.EventManagement.TicketPlaceDef])
getEventManagementTicketdashboardTicketplaceDrafts a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.getEventManagementTicketdashboardTicketplaceDrafts a6 a5 a3 a2 a1

postEventManagementTicketdashboardTicketplaceRecommend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> [API.Types.Dashboard.AppManagement.EventManagement.RecommendToggleReq] -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceRecommend a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceRecommend a4 a3 a1
