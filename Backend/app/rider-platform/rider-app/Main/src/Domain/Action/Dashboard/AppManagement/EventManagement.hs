{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.EventManagement
  ( getEventManagementTicketdashboardTicketplaceDef,
    postEventManagementTicketdashboardTicketplaceCleardraft,
    postEventManagementTicketdashboardTicketplaceSubmitDraft,
    postEventManagementTicketdashboardTicketplaceApproveDraft,
    postEventManagementTicketdashboardTicketplaceCreate,
    postEventManagementTicketdashboardTicketplaceUpdateBasicInfo,
    postEventManagementTicketdashboardTicketplaceUpdateService,
    postEventManagementTicketdashboardTicketplaceDelService,
    postEventManagementTicketdashboardTicketplaceServiceUpdateCategory,
    postEventManagementTicketdashboardTicketplaceServiceDelCategory,
    postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople,
    postEventManagementTicketdashboardTicketPlaceCategoryDelPeople,
  )
where

import Control.Monad.Extra (concatMapM)
import Data.List (nub)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BusinessHour as DBusinessHour
import qualified Domain.Types.DraftTicketChange as DDTC
import qualified Domain.Types.EventManagement as DEM
import qualified "this" Domain.Types.EventManagement
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServiceCategory as DServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import qualified Domain.Types.ServicePeopleCategory as DServicePeopleCategory
import qualified Domain.Types.TicketPlace as DTicketPlace
import qualified "this" Domain.Types.TicketPlace
import qualified Domain.Types.TicketService as DTicketService
import qualified "this" Domain.Types.TicketService
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BusinessHour as QBusinessHour
import qualified Storage.Queries.DraftTicketChange as QDTC
import qualified Storage.Queries.ServiceCategory as QServiceCategory
import qualified Storage.Queries.ServicePeopleCategory as QServicePeopleCategory
import qualified Storage.Queries.TicketPlace as QTicketPlace
import qualified Storage.Queries.TicketService as QTicketService
import Tools.Auth
import Tools.Error

getEventManagementTicketdashboardTicketplaceDef :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
getEventManagementTicketdashboardTicketplaceDef _merchantShortId _opCity ticketPlaceId requestorId _requestorRole = do
  checkAccess ticketPlaceId requestorId _requestorRole
  getTicketDef ticketPlaceId

postEventManagementTicketdashboardTicketplaceCleardraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCleardraft _merchantShortId _opCity ticketPlaceId requestorId requestorRole = do
  checkAccess ticketPlaceId requestorId requestorRole
  QDTC.deleteDraftById ticketPlaceId
  return Kernel.Types.APISuccess.Success

postEventManagementTicketdashboardTicketplaceSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceSubmitDraft _merchantShortId _opCity ticketPlaceId requestorId requestorRole = do error "Logic yet to be decided" ticketPlaceId requestorId requestorRole

postEventManagementTicketdashboardTicketplaceApproveDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceApproveDraft _merchantShortId _opCity ticketPlaceId requestorId requestorRole = do error "Logic yet to be decided" ticketPlaceId requestorId requestorRole

postEventManagementTicketdashboardTicketplaceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceCreate _merchantShortId _opCity requestorId requestorRole req = do
  ticketPlaceDef <- newTicketPlaceDef requestorId requestorRole req
  return ticketPlaceDef

postEventManagementTicketdashboardTicketplaceUpdateBasicInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateBasicInfo _merchantShortId _opCity ticketPlaceId requestorId requestorRole req = do
  checkAccess ticketPlaceId requestorId requestorRole
  updateBasicInfo ticketPlaceId req

postEventManagementTicketdashboardTicketplaceUpdateService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.TicketServiceDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceUpdateService _merchantShortId _opCity ticketPlaceId requestorId requestorRole req = do
  checkAccess ticketPlaceId requestorId requestorRole
  upsertServiceDef ticketPlaceId req

postEventManagementTicketdashboardTicketplaceDelService :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceDelService _merchantShortId _opCity ticketPlaceId serviceId requestorId requestorRole = do
  checkAccess ticketPlaceId requestorId requestorRole
  delServiceDef ticketPlaceId serviceId

postEventManagementTicketdashboardTicketplaceServiceUpdateCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceUpdateCategory _merchantShortId _opCity ticketPlaceId serviceId requestorId requestorRole req = do
  checkAccess ticketPlaceId requestorId requestorRole
  upsertServiceCategoryDef ticketPlaceId serviceId req

postEventManagementTicketdashboardTicketplaceServiceDelCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceServiceDelCategory _merchantShortId _opCity ticketPlaceId serviceId categoryId requestorId requestorRole = do
  checkAccess ticketPlaceId requestorId requestorRole
  delServiceCategoryDef ticketPlaceId serviceId categoryId

postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople _merchantShortId _opCity ticketPlaceId categoryId requestorId requestorRole req = do
  checkAccess ticketPlaceId requestorId requestorRole
  upsertServicePeopleCategoryDef ticketPlaceId categoryId req

postEventManagementTicketdashboardTicketPlaceCategoryDelPeople :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketPlaceCategoryDelPeople _merchantShortId _opCity ticketPlaceId categoryId peopleId requestorId requestorRole = do
  checkAccess ticketPlaceId requestorId requestorRole
  delServicePeopleCategoryDef ticketPlaceId categoryId peopleId

newTicketPlaceDef :: Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow DEM.TicketPlaceDef
newTicketPlaceDef requestorId requestorRole basicInfo = do
  reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- fromMaybeM (InvalidRequest "RequestorRole is required") requestorRole
  now <- getCurrentTime
  ticketPlaceId <- generateGUID
  let ticketDef =
        DEM.TicketPlaceDef
          { id = ticketPlaceId,
            basicInformation = basicInfo,
            services = [],
            serviceCategories = [],
            servicePeopleCategories = [],
            isDraft = True
          }
  let draftTicketChange =
        DDTC.DraftTicketChange
          { id = ticketPlaceId,
            draftPayload = Just ticketDef,
            isApprovalRequired = False,
            status = DDTC.OPEN,
            ticketMerchantId = case reqRole of
              Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT -> Just reqId
              _ -> Nothing,
            merchantId = Nothing,
            merchantOperatingCityId = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QDTC.create draftTicketChange
  return ticketDef

getTicketDef :: Id DTicketPlace.TicketPlace -> Environment.Flow DEM.TicketPlaceDef
getTicketDef placeId = do
  mbDraftChange <- QDTC.findById placeId
  case mbDraftChange >>= (.draftPayload) of
    Just ticketPlaceDef -> return $ ticketPlaceDef {DEM.isDraft = True}
    Nothing -> do
      ticketPlace <- QTicketPlace.findById placeId >>= fromMaybeM (InvalidRequest $ "Ticket place not found: " <> getId placeId)
      services <- QTicketService.getTicketServicesByPlaceId placeId.getId
      let linkedBusinessHourIds = concatMap (.businessHours) services
      linkedBusinessHours <- catMaybes <$> mapM QBusinessHour.findById linkedBusinessHourIds
      let linkedServiceCategoryIds = concatMap (.categoryId) linkedBusinessHours
      linkedServiceCategories <- catMaybes <$> mapM QServiceCategory.findById linkedServiceCategoryIds
      let linkedServicePeopleCategoryIds = concatMap (.peopleCategory) linkedServiceCategories
      linkedServicePeopleCategories <- concatMapM QServicePeopleCategory.findAllById linkedServicePeopleCategoryIds
      let basicInfo =
            DEM.BasicInformation
              { name = ticketPlace.name,
                description = ticketPlace.description,
                shortDesc = ticketPlace.shortDesc,
                address = Nothing,
                latitude = ticketPlace.lat,
                longitude = ticketPlace.lon,
                status = ticketPlace.status,
                priority = ticketPlace.priority,
                placeType = ticketPlace.placeType,
                allowSameDayBooking = ticketPlace.allowSameDayBooking,
                gallery = ticketPlace.gallery,
                iconUrl = ticketPlace.iconUrl,
                mapImageUrl = ticketPlace.mapImageUrl,
                termsAndConditions = ticketPlace.termsAndConditions,
                termsAndConditionsUrl = ticketPlace.termsAndConditionsUrl,
                openTimings = ticketPlace.openTimings,
                closeTimings = ticketPlace.closeTimings,
                customTabs = ticketPlace.customTabs
              }
          serviceDefs = map (toTicketServiceDef linkedBusinessHours) services
          serviceCategoryDefs = map (toServiceCategoryDef linkedBusinessHours) linkedServiceCategories
          servicePeopleCategoryDefs = map toServicePeopleCategoryDef linkedServicePeopleCategories
      let ticketDef =
            DEM.TicketPlaceDef
              { id = ticketPlace.id,
                basicInformation = basicInfo,
                services = serviceDefs,
                serviceCategories = serviceCategoryDefs,
                servicePeopleCategories = servicePeopleCategoryDefs,
                isDraft = False
              }
      now <- getCurrentTime
      let draftTicketChange =
            DDTC.DraftTicketChange
              { id = ticketPlace.id,
                draftPayload = Just ticketDef,
                isApprovalRequired = False,
                status = DDTC.OPEN,
                ticketMerchantId = ticketPlace.ticketMerchantId,
                merchantId = Nothing,
                merchantOperatingCityId = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QDTC.create draftTicketChange
      return ticketDef
  where
    toTicketServiceDef :: [DBusinessHour.BusinessHour] -> DTicketService.TicketService -> DEM.TicketServiceDef
    toTicketServiceDef linkedBusinessHours svc =
      DEM.TicketServiceDef
        { id = svc.id,
          service = svc.service,
          shortDesc = svc.shortDesc,
          operationalDays = svc.operationalDays,
          operationalDate = svc.operationalDate,
          maxVerification = svc.maxVerification,
          allowFutureBooking = svc.allowFutureBooking,
          allowCancellation = svc.allowCancellation,
          expiry = svc.expiry,
          serviceCategoryId = nub $ concatMap (.categoryId) $ filter (\bh -> any (\bhId -> bhId == bh.id) svc.businessHours) linkedBusinessHours
        }
    toServiceCategoryDef :: [DBusinessHour.BusinessHour] -> DServiceCategory.ServiceCategory -> DEM.ServiceCategoryDef
    toServiceCategoryDef linkedBusinessHours sc =
      DEM.ServiceCategoryDef
        { id = sc.id,
          name = sc.name,
          description = sc.description,
          allowedSeats = sc.allowedSeats,
          businessHours = nub $ map toBusinessHourDef $ filter (\bh -> any (\cid -> cid == sc.id) bh.categoryId) linkedBusinessHours,
          peopleCategory = sc.peopleCategory
        }
    toBusinessHourDef :: DBusinessHour.BusinessHour -> DEM.BusinessHourDef
    toBusinessHourDef bh =
      DEM.BusinessHourDef
        { btype = bh.btype,
          bookingClosingTime = bh.bookingClosingTime
        }
    toServicePeopleCategoryDef :: DServicePeopleCategory.ServicePeopleCategory -> DEM.ServicePeopleCategoryDef
    toServicePeopleCategoryDef spc =
      DEM.ServicePeopleCategoryDef
        { id = spc.id,
          name = spc.name,
          description = spc.description,
          pricingType = spc.pricingType,
          priceAmount = spc.pricePerUnit.amount,
          priceCurrency = spc.pricePerUnit.currency
        }

---- draft updations -----
upsertServiceDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.TicketServiceDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
upsertServiceDef ticketPlaceId serviceDef = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedServices =
        if any (\s -> s.id == serviceDef.id) ticketDef.services
          then map (\s -> if s.id == serviceDef.id then serviceDef else s) ticketDef.services
          else serviceDef : ticketDef.services
  let updatedTicketDef = ticketDef {DEM.services = updatedServices}
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

delServiceDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
delServiceDef ticketPlaceId serviceId = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedServices = filter (\s -> s.id /= serviceId) ticketDef.services
  let updatedTicketDef = ticketDef {DEM.services = updatedServices}
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

updateBasicInfo :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
updateBasicInfo ticketPlaceId basicInfo = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedTicketDef = ticketDef {DEM.basicInformation = basicInfo}
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

upsertServiceCategoryDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
upsertServiceCategoryDef ticketPlaceId serviceId categoryDef = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedCategories =
        if any (\c -> c.id == categoryDef.id) ticketDef.serviceCategories
          then map (\c -> if c.id == categoryDef.id then categoryDef else c) ticketDef.serviceCategories
          else categoryDef : ticketDef.serviceCategories
  -- Update serviceCategoryId in the specific service
  let updatedServices =
        map
          ( \s ->
              if s.id == serviceId
                then
                  s
                    { DEM.serviceCategoryId =
                        if not (any (\c -> c.id == categoryDef.id) ticketDef.serviceCategories)
                          then categoryDef.id : s.serviceCategoryId
                          else s.serviceCategoryId
                    }
                else s
          )
          ticketDef.services
  let updatedTicketDef =
        ticketDef
          { DEM.serviceCategories = updatedCategories,
            DEM.services = updatedServices
          }
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

delServiceCategoryDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
delServiceCategoryDef ticketPlaceId serviceId categoryId = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedCategories = filter (\c -> c.id /= categoryId) ticketDef.serviceCategories
  -- Remove categoryId from the specific service
  let updatedServices =
        map
          ( \s ->
              if s.id == serviceId
                then s {DEM.serviceCategoryId = filter (/= categoryId) s.serviceCategoryId}
                else s
          )
          ticketDef.services
  let updatedTicketDef =
        ticketDef
          { DEM.serviceCategories = updatedCategories,
            DEM.services = updatedServices
          }
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

upsertServicePeopleCategoryDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
upsertServicePeopleCategoryDef ticketPlaceId categoryId peopleDef = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedPeopleCategories =
        if any (\p -> p.id == peopleDef.id) ticketDef.servicePeopleCategories
          then map (\p -> if p.id == peopleDef.id then peopleDef else p) ticketDef.servicePeopleCategories
          else peopleDef : ticketDef.servicePeopleCategories
  -- Update peopleCategory in the specific service category
  let updatedServiceCategories =
        map
          ( \sc ->
              if sc.id == categoryId
                then
                  sc
                    { DEM.peopleCategory =
                        if not (any (\p -> p.id == peopleDef.id) ticketDef.servicePeopleCategories)
                          then peopleDef.id : sc.peopleCategory
                          else sc.peopleCategory
                    }
                else sc
          )
          ticketDef.serviceCategories
  let updatedTicketDef =
        ticketDef
          { DEM.servicePeopleCategories = updatedPeopleCategories,
            DEM.serviceCategories = updatedServiceCategories
          }
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

delServicePeopleCategoryDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
delServicePeopleCategoryDef ticketPlaceId categoryId peopleId = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedPeopleCategories = filter (\p -> p.id /= peopleId) ticketDef.servicePeopleCategories
  -- Remove peopleId from the specific service category
  let updatedServiceCategories =
        map
          ( \sc ->
              if sc.id == categoryId
                then sc {DEM.peopleCategory = filter (/= peopleId) sc.peopleCategory}
                else sc
          )
          ticketDef.serviceCategories
  let updatedTicketDef =
        ticketDef
          { DEM.servicePeopleCategories = updatedPeopleCategories,
            DEM.serviceCategories = updatedServiceCategories
          }
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

----------------- Mostly utils will move them later to another file
checkAccess :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow ()
checkAccess ticketPlaceId requestorId _requestorRole = do
  _reqId <- fromMaybeM (InvalidRequest "RequestorId is required") requestorId
  reqRole <- _requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  case reqRole of
    Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN -> pure ()
    Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT -> do
      ticketMid <- getTicketPlaceMid ticketPlaceId
      unless (ticketMid == requestorId) $ throwError $ InvalidRequest "Requestor does not have this access"
    _ -> throwError $ InvalidRequest "Requestor does not have this access"

getTicketPlaceMid :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow (Maybe Text)
getTicketPlaceMid ticketPlaceId = do
  mbTicketPlace <- QTicketPlace.findById ticketPlaceId
  case mbTicketPlace of
    Just tp -> return tp.ticketMerchantId
    Nothing -> do
      mbDraftChange <- QDTC.findById ticketPlaceId
      return $ mbDraftChange >>= (.ticketMerchantId)
