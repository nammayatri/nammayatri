module Domain.Action.RiderPlatform.AppManagement.EventManagement
  ( getEventManagementTicketdashboardTicketplaceDef,
    postEventManagementTicketdashboardTicketplaceCleardraft,
    postEventManagementTicketdashboardTicketplaceSubmitDraft,
    postEventManagementTicketdashboardTicketplaceReviewDraft,
    postEventManagementTicketdashboardTicketplaceCreate,
    postEventManagementTicketdashboardTicketplaceUpdateBasicInfo,
    postEventManagementTicketdashboardTicketplaceUpdateService,
    postEventManagementTicketdashboardTicketplaceDelService,
    postEventManagementTicketdashboardTicketplaceServiceUpdateCategory,
    postEventManagementTicketdashboardTicketplaceServiceDelCategory,
    postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople,
    postEventManagementTicketdashboardTicketPlaceCategoryDelPeople,
    postEventManagementTicketdashboardTicketplaceCancelSubmitDraft,
    getEventManagementTicketdashboardTicketplaceDrafts,
    postEventManagementTicketdashboardTicketplaceRecommend,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.EventManagement
import "rider-app" Domain.Types.AccessMatrix
import qualified "rider-app" Domain.Types.DraftTicketChange
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
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getEventManagementTicketdashboardTicketplaceDef :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
getEventManagementTicketdashboardTicketplaceDef merchantShortId opCity apiTokenInfo ticketPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.getEventManagementTicketdashboardTicketplaceDef) ticketPlaceId

postEventManagementTicketdashboardTicketplaceCleardraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCleardraft merchantShortId opCity apiTokenInfo ticketPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceCleardraft) ticketPlaceId

postEventManagementTicketdashboardTicketplaceSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceSubmitDraft) ticketPlaceId

postEventManagementTicketdashboardTicketplaceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceCreate) req

postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateBasicInfo merchantShortId opCity apiTokenInfo ticketPlaceId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceUpdateBasicInfo) ticketPlaceId req

postEventManagementTicketdashboardTicketplaceUpdateService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.TicketServiceDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateService merchantShortId opCity apiTokenInfo ticketPlaceId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceUpdateService) ticketPlaceId req

postEventManagementTicketdashboardTicketplaceDelService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceDelService merchantShortId opCity apiTokenInfo ticketPlaceId serviceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceDelService) ticketPlaceId serviceId

postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceUpdateCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceServiceUpdateCategory) ticketPlaceId serviceId req

postEventManagementTicketdashboardTicketplaceServiceDelCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceDelCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId categoryId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceServiceDelCategory) ticketPlaceId serviceId categoryId

postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople) ticketPlaceId categoryId req

postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryDelPeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId peopleId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketPlaceCategoryDelPeople) ticketPlaceId categoryId peopleId

postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCancelSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceCancelSubmitDraft) ticketPlaceId

postEventManagementTicketdashboardTicketplaceReviewDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.Dashboard.AppManagement.EventManagement.ReviewDraftReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceReviewDraft merchantShortId opCity apiTokenInfo ticketPlaceId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceReviewDraft) ticketPlaceId req

getEventManagementTicketdashboardTicketplaceDrafts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Domain.Types.DraftTicketChange.DraftStatus -> Environment.Flow [Domain.Types.EventManagement.TicketPlaceDef])
getEventManagementTicketdashboardTicketplaceDrafts merchantShortId opCity apiTokenInfo limit offset status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.getEventManagementTicketdashboardTicketplaceDrafts) limit offset status

postEventManagementTicketdashboardTicketplaceRecommend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> [API.Types.Dashboard.AppManagement.EventManagement.RecommendToggleReq] -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceRecommend merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceRecommend) req
