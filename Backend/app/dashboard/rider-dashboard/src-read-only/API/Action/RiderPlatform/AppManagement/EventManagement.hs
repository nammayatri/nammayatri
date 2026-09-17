{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.EventManagement
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.EventManagement
import qualified Domain.Action.RiderPlatform.AppManagement.EventManagement
import qualified Domain.Types.DraftTicketChange
import qualified "rider-app" Domain.Types.EventManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import qualified "rider-app" Domain.Types.TicketPlace
import qualified "rider-app" Domain.Types.TicketService
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (GetEventManagementTicketdashboardTicketplaceDef :<|> PostEventManagementTicketdashboardTicketplaceCleardraft :<|> PostEventManagementTicketdashboardTicketplaceSubmitDraft :<|> PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft :<|> PostEventManagementTicketdashboardTicketplaceReviewDraft :<|> PostEventManagementTicketdashboardTicketplaceCreate :<|> PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo :<|> PostEventManagementTicketdashboardTicketplaceUpdateService :<|> PostEventManagementTicketdashboardTicketplaceDelService :<|> PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory :<|> PostEventManagementTicketdashboardTicketplaceServiceDelCategory :<|> PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :<|> PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople :<|> GetEventManagementTicketdashboardTicketplaceDrafts :<|> PostEventManagementTicketdashboardTicketplaceRecommend)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getEventManagementTicketdashboardTicketplaceDef merchantId city :<|> postEventManagementTicketdashboardTicketplaceCleardraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceSubmitDraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceCancelSubmitDraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceReviewDraft merchantId city :<|> postEventManagementTicketdashboardTicketplaceCreate merchantId city :<|> postEventManagementTicketdashboardTicketplaceUpdateBasicInfo merchantId city :<|> postEventManagementTicketdashboardTicketplaceUpdateService merchantId city :<|> postEventManagementTicketdashboardTicketplaceDelService merchantId city :<|> postEventManagementTicketdashboardTicketplaceServiceUpdateCategory merchantId city :<|> postEventManagementTicketdashboardTicketplaceServiceDelCategory merchantId city :<|> postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople merchantId city :<|> postEventManagementTicketdashboardTicketPlaceCategoryDelPeople merchantId city :<|> getEventManagementTicketdashboardTicketplaceDrafts merchantId city :<|> postEventManagementTicketdashboardTicketplaceRecommend merchantId city

type GetEventManagementTicketdashboardTicketplaceDef =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEF)
      :> API.Types.Dashboard.AppManagement.EventManagement.GetEventManagementTicketdashboardTicketplaceDef
  )

type PostEventManagementTicketdashboardTicketplaceCleardraft =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CLEARDRAFT)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceCleardraft
  )

type PostEventManagementTicketdashboardTicketplaceSubmitDraft =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SUBMIT_DRAFT)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceSubmitDraft
  )

type PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CANCEL_SUBMIT_DRAFT)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft
  )

type PostEventManagementTicketdashboardTicketplaceReviewDraft =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_REVIEW_DRAFT)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceReviewDraft
  )

type PostEventManagementTicketdashboardTicketplaceCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CREATE)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceCreate
  )

type PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_BASIC_INFO)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo
  )

type PostEventManagementTicketdashboardTicketplaceUpdateService =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_SERVICE)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceUpdateService
  )

type PostEventManagementTicketdashboardTicketplaceDelService =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEL_SERVICE)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceDelService
  )

type PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_UPDATE_CATEGORY)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory
  )

type PostEventManagementTicketdashboardTicketplaceServiceDelCategory =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_DEL_CATEGORY)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceServiceDelCategory
  )

type PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_UPDATE_PEOPLE)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople
  )

type PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_DEL_PEOPLE)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople
  )

type GetEventManagementTicketdashboardTicketplaceDrafts =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DRAFTS)
      :> API.Types.Dashboard.AppManagement.EventManagement.GetEventManagementTicketdashboardTicketplaceDrafts
  )

type PostEventManagementTicketdashboardTicketplaceRecommend =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EVENT_MANAGEMENT / 'API.Types.Dashboard.AppManagement.EventManagement.POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_RECOMMEND)
      :> API.Types.Dashboard.AppManagement.EventManagement.PostEventManagementTicketdashboardTicketplaceRecommend
  )

getEventManagementTicketdashboardTicketplaceDef :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
getEventManagementTicketdashboardTicketplaceDef merchantShortId opCity apiTokenInfo ticketPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.getEventManagementTicketdashboardTicketplaceDef merchantShortId opCity apiTokenInfo ticketPlaceId

postEventManagementTicketdashboardTicketplaceCleardraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCleardraft merchantShortId opCity apiTokenInfo ticketPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceCleardraft merchantShortId opCity apiTokenInfo ticketPlaceId

postEventManagementTicketdashboardTicketplaceSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId

postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCancelSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceCancelSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId

postEventManagementTicketdashboardTicketplaceReviewDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.EventManagement.ReviewDraftReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceReviewDraft merchantShortId opCity apiTokenInfo ticketPlaceId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceReviewDraft merchantShortId opCity apiTokenInfo ticketPlaceId req

postEventManagementTicketdashboardTicketplaceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.EventManagement.BasicInformation -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceCreate merchantShortId opCity apiTokenInfo req

postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.BasicInformation -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateBasicInfo merchantShortId opCity apiTokenInfo ticketPlaceId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceUpdateBasicInfo merchantShortId opCity apiTokenInfo ticketPlaceId req

postEventManagementTicketdashboardTicketplaceUpdateService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.TicketServiceDef -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateService merchantShortId opCity apiTokenInfo ticketPlaceId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceUpdateService merchantShortId opCity apiTokenInfo ticketPlaceId req

postEventManagementTicketdashboardTicketplaceDelService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceDelService merchantShortId opCity apiTokenInfo ticketPlaceId serviceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceDelService merchantShortId opCity apiTokenInfo ticketPlaceId serviceId

postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceUpdateCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceServiceUpdateCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId req

postEventManagementTicketdashboardTicketplaceServiceDelCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceDelCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId categoryId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceServiceDelCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId categoryId

postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId req

postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Environment.FlowHandler Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryDelPeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId peopleId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketPlaceCategoryDelPeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId peopleId

getEventManagementTicketdashboardTicketplaceDrafts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Domain.Types.DraftTicketChange.DraftStatus -> Environment.FlowHandler [Domain.Types.EventManagement.TicketPlaceDef])
getEventManagementTicketdashboardTicketplaceDrafts merchantShortId opCity apiTokenInfo limit offset status = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.getEventManagementTicketdashboardTicketplaceDrafts merchantShortId opCity apiTokenInfo limit offset status

postEventManagementTicketdashboardTicketplaceRecommend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> [API.Types.Dashboard.AppManagement.EventManagement.RecommendToggleReq] -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceRecommend merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EventManagement.postEventManagementTicketdashboardTicketplaceRecommend merchantShortId opCity apiTokenInfo req
