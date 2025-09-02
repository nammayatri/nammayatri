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
import Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding (getDashboardAccessType)
import qualified "rider-app" Domain.Types.DraftTicketChange
import qualified "rider-app" Domain.Types.EventManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
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
import Tools.Auth.Api
import Tools.Auth.Merchant

getEventManagementTicketdashboardTicketplaceDef :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
getEventManagementTicketdashboardTicketplaceDef merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.getEventManagementTicketdashboardTicketplaceDef) ticketPlaceId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketplaceCleardraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCleardraft merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceCleardraft) ticketPlaceId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketplaceSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceSubmitDraft) ticketPlaceId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketplaceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceCreate merchantShortId opCity apiTokenInfo _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceCreate) (Just requestorId) (Just requestorRole) req

postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateBasicInfo merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceUpdateBasicInfo) ticketPlaceId (Just requestorId) (Just requestorRole) req

postEventManagementTicketdashboardTicketplaceUpdateService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.TicketServiceDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateService merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceUpdateService) ticketPlaceId (Just requestorId) (Just requestorRole) req

postEventManagementTicketdashboardTicketplaceDelService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceDelService merchantShortId opCity apiTokenInfo ticketPlaceId serviceId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceDelService) ticketPlaceId serviceId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceUpdateCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceServiceUpdateCategory) ticketPlaceId serviceId (Just requestorId) (Just requestorRole) req

postEventManagementTicketdashboardTicketplaceServiceDelCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceDelCategory merchantShortId opCity apiTokenInfo ticketPlaceId serviceId categoryId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceServiceDelCategory) ticketPlaceId serviceId categoryId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople) ticketPlaceId categoryId (Just requestorId) (Just requestorRole) req

postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryDelPeople merchantShortId opCity apiTokenInfo ticketPlaceId categoryId peopleId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketPlaceCategoryDelPeople) ticketPlaceId categoryId peopleId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCancelSubmitDraft merchantShortId opCity apiTokenInfo ticketPlaceId _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceCancelSubmitDraft) ticketPlaceId (Just requestorId) (Just requestorRole)

postEventManagementTicketdashboardTicketplaceReviewDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.EventManagement.ReviewDraftReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceReviewDraft merchantShortId opCity apiTokenInfo ticketPlaceId _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceReviewDraft) ticketPlaceId (Just requestorId) (Just requestorRole) req

getEventManagementTicketdashboardTicketplaceDrafts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Domain.Types.DraftTicketChange.DraftStatus -> Environment.Flow [Domain.Types.EventManagement.TicketPlaceDef])
getEventManagementTicketdashboardTicketplaceDrafts merchantShortId opCity apiTokenInfo _ _ limit offset status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.getEventManagementTicketdashboardTicketplaceDrafts) (Just requestorId) (Just requestorRole) limit offset status

postEventManagementTicketdashboardTicketplaceRecommend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> [API.Types.Dashboard.AppManagement.EventManagement.RecommendToggleReq] -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceRecommend merchantShortId opCity apiTokenInfo _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.eventManagementDSL.postEventManagementTicketdashboardTicketplaceRecommend) (Just requestorId) (Just requestorRole) req
