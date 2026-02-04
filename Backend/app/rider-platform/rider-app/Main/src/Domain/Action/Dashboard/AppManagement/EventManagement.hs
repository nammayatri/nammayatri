module Domain.Action.Dashboard.AppManagement.EventManagement
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
    getEventManagementTicketdashboardTicketplaceDrafts,
    postEventManagementTicketdashboardTicketplaceCancelSubmitDraft,
    postEventManagementTicketdashboardTicketplaceRecommend,
    getTicketDef,
    checkAccess,
    invalidateTicketPlaceAvailabilityCache,
  )
where

import qualified API.Types.Dashboard.AppManagement.Endpoints.EventManagement as Review
import qualified API.Types.Dashboard.AppManagement.EventManagement
import API.Types.Dashboard.AppManagement.TicketDashboard as DTD
import Control.Monad.Extra (concatMapM)
import Data.List (nub, nubBy, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Domain.Action.Dashboard.AppManagement.EventManagement.Utils (deleteAsset)
import qualified Domain.Types.BusinessHour as DBusinessHour
import qualified Domain.Types.DraftTicketChange
import qualified Domain.Types.DraftTicketChange as DDTC
import qualified Domain.Types.DraftTicketChangeHistory as DTCH
import qualified Domain.Types.EventManagement as DEM
import qualified "this" Domain.Types.EventManagement
import qualified Domain.Types.EventManagement.Permissions as Permissions
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified Domain.Types.MerchantOperatingCity
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
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.BusinessHour as QBusinessHour
import qualified Storage.Queries.DraftTicketChange as QDTC
import qualified Storage.Queries.DraftTicketChangeHistory as QDTCH
import qualified Storage.Queries.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.ServiceCategory as QServiceCategory
import qualified Storage.Queries.ServicePeopleCategory as QServicePeopleCategory
import qualified Storage.Queries.TicketPlace as QTicketPlace
import qualified Storage.Queries.TicketService as QTicketService
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
postEventManagementTicketdashboardTicketplaceSubmitDraft _merchantShortId _opCity ticketPlaceId requestorId requestorRole = do
  checkAccess ticketPlaceId requestorId requestorRole
  mbTicketPlace <- QTicketPlace.findById ticketPlaceId
  case mbTicketPlace of
    Just _ -> do
      ticketDef <- getTicketDef ticketPlaceId
      oldTicketDef <- getLiveTicketDef ticketPlaceId
      unless (Just ticketDef /= oldTicketDef) $ throwError $ InvalidRequest "No changes to submit"
      draftChange <- QDTC.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "No draft found")
      m <- findMerchantByShortId _merchantShortId
      moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
      riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = moCity.id.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
      let approvalRequired = case oldTicketDef of
            Just mo -> Permissions.needsApproval riderConfig.ticketingPermissionConfig mo ticketDef
            Nothing -> True
      if approvalRequired
        then do
          QDTC.updateStatus DDTC.APPROVAL_PENDING ticketPlaceId
          QDTC.updateMessage Nothing ticketPlaceId
        else do
          applyDraftChanges draftChange
          moveDraftToHistory draftChange DDTC.ACCEPTED Nothing
          QDTC.deleteDraftById ticketPlaceId
    Nothing -> do
      QDTC.updateStatus DDTC.APPROVAL_PENDING ticketPlaceId
      QDTC.updateMessage Nothing ticketPlaceId
  return Kernel.Types.APISuccess.Success

postEventManagementTicketdashboardTicketplaceReviewDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.EventManagement.ReviewDraftReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceReviewDraft _merchantShortId _opCity ticketPlaceId requestorId requestorRole req = do
  _reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN) $ throwError $ InvalidRequest "Requestor does not have this access"
  draftChange <- QDTC.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "No draft found")
  unless (draftChange.status == DDTC.APPROVAL_PENDING) $ throwError $ InvalidRequest "Ticket is not in approval pending state"
  if req.status == Review.APPROVE
    then do
      applyDraftChanges draftChange
      moveDraftToHistory draftChange DDTC.ACCEPTED requestorId
      QDTC.deleteDraftById ticketPlaceId
    else do
      moveDraftToHistory draftChange DDTC.REJECTED requestorId
      QDTC.updateStatus DDTC.REJECTED ticketPlaceId
      QDTC.updateMessage req.message ticketPlaceId
  return Kernel.Types.APISuccess.Success

postEventManagementTicketdashboardTicketplaceCancelSubmitDraft :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceCancelSubmitDraft _merchantShortId _opCity ticketPlaceId requestorId requestorRole = do
  checkAccess ticketPlaceId requestorId requestorRole
  draftChange <- QDTC.findById ticketPlaceId >>= fromMaybeM (InvalidRequest "No draft found")
  unless (draftChange.status == DDTC.APPROVAL_PENDING) $ throwError $ InvalidRequest "Ticket is not in approval pending state"
  QDTC.updateStatus DDTC.OPEN ticketPlaceId
  return Kernel.Types.APISuccess.Success

postEventManagementTicketdashboardTicketplaceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef)
postEventManagementTicketdashboardTicketplaceCreate _merchantShortId _opCity requestorId requestorRole req = do
  m <- findMerchantByShortId _merchantShortId
  moCity <- CQMOC.findByMerchantIdAndCity m.id m.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show m.defaultCity)
  ticketPlaceDef <- newTicketPlaceDef requestorId requestorRole req m.id moCity.id
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

getEventManagementTicketdashboardTicketplaceDrafts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Domain.Types.DraftTicketChange.DraftStatus -> Environment.Flow [Domain.Types.EventManagement.TicketPlaceDef])
getEventManagementTicketdashboardTicketplaceDrafts _merchantShortId _opCity requestorId requestorRole limit offset status = do
  _reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN) $ throwError $ InvalidRequest "Requestor does not have this access"
  drafts <- QDTC.findAllByStatus (pure limit) (pure offset) status
  return $ catMaybes $ map (.draftPayload) drafts

postEventManagementTicketdashboardTicketplaceRecommend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> [API.Types.Dashboard.AppManagement.EventManagement.RecommendToggleReq] -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postEventManagementTicketdashboardTicketplaceRecommend _merchantShortId _opCity requestorId requestorRole req = do
  _reqId <- requestorId & fromMaybeM (InvalidRequest "RequestorId is required")
  reqRole <- requestorRole & fromMaybeM (InvalidRequest "RequestorRole is required")
  unless (reqRole == Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN) $ throwError $ InvalidRequest "Requestor does not have this access"
  forM_ req $ \r -> QTicketPlace.updateRecommendById r.recommend r.placeId
  m <- findMerchantByShortId _merchantShortId
  let redisKey = makeRecommendedTicketPlacesKey m.id
  _ <- Hedis.del redisKey
  return Kernel.Types.APISuccess.Success
  where
    makeRecommendedTicketPlacesKey :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Text
    makeRecommendedTicketPlacesKey merchantId = "AttractionRecommend:mid-" <> merchantId.getId

newTicketPlaceDef :: Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Domain.Types.EventManagement.BasicInformation -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Environment.Flow DEM.TicketPlaceDef
newTicketPlaceDef requestorId requestorRole basicInfo merchantId merchantOperatingCityId = do
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
            isApprovalRequired = True,
            status = DDTC.OPEN,
            ticketMerchantId = case reqRole of
              Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT -> Just reqId
              _ -> Nothing,
            message = Nothing,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QDTC.create draftTicketChange
  return ticketDef

getLiveTicketDef :: Id DTicketPlace.TicketPlace -> Environment.Flow (Maybe DEM.TicketPlaceDef)
getLiveTicketDef placeId = do
  mbTicketPlace <- QTicketPlace.findById placeId
  case mbTicketPlace of
    Nothing -> return Nothing
    Just ticketPlace -> do
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
                customTabs = ticketPlace.customTabs,
                rules = ticketPlace.rules,
                faqs = ticketPlace.faqs,
                metadata = ticketPlace.metadata,
                isRecurring = Just ticketPlace.isRecurring,
                platformFee = ticketPlace.platformFee,
                platformFeeVendor = ticketPlace.platformFeeVendor,
                pricingOnwards = ticketPlace.pricingOnwards,
                endDate = ticketPlace.endDate,
                isClosed = ticketPlace.isClosed,
                startDate = ticketPlace.startDate,
                venue = ticketPlace.venue,
                assignTicketToBpp = ticketPlace.assignTicketToBpp,
                enforcedAsSubPlace = ticketPlace.enforcedAsSubPlace
              }
          serviceDefs = map (toTicketServiceDef linkedBusinessHours) services
          serviceCategoryDefs = map (toServiceCategoryDef linkedBusinessHours) linkedServiceCategories
          servicePeopleCategoryDefs = nub $ map toServicePeopleCategoryDef linkedServicePeopleCategories
      return $
        pure $
          DEM.TicketPlaceDef
            { id = ticketPlace.id,
              basicInformation = basicInfo,
              services = serviceDefs,
              serviceCategories = serviceCategoryDefs,
              servicePeopleCategories = servicePeopleCategoryDefs,
              isDraft = False
            }
  where
    toTicketServiceDef :: [DBusinessHour.BusinessHour] -> DTicketService.TicketService -> DEM.TicketServiceDef
    toTicketServiceDef linkedBusinessHours svc =
      DEM.TicketServiceDef
        { id = svc.id,
          service = svc.service,
          shortDesc = svc.shortDesc,
          serviceDetails = svc.serviceDetails,
          operationalDays = svc.operationalDays,
          operationalDate = svc.operationalDate,
          maxVerification = svc.maxVerification,
          allowFutureBooking = svc.allowFutureBooking,
          allowCancellation = svc.allowCancellation,
          expiry = svc.expiry,
          serviceCategoryId = nub $ concatMap (.categoryId) $ filter (\bh -> any (\bhId -> bhId == bh.id) svc.businessHours) linkedBusinessHours,
          rules = svc.rules,
          subPlaceId = svc.subPlaceId,
          maxSelection = svc.maxSelection
        }
    toServiceCategoryDef :: [DBusinessHour.BusinessHour] -> DServiceCategory.ServiceCategory -> DEM.ServiceCategoryDef
    toServiceCategoryDef linkedBusinessHours sc =
      DEM.ServiceCategoryDef
        { id = sc.id,
          name = sc.name,
          description = sc.description,
          inclusionPoints = sc.inclusionPoints,
          allowedSeats = sc.allowedSeats,
          businessHours = nub $ map toBusinessHourDef $ filter (\bh -> any (\cid -> cid == sc.id) bh.categoryId) linkedBusinessHours,
          peopleCategory = sc.peopleCategory,
          rules = sc.rules,
          maxSelection = sc.maxSelection
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
          priceCurrency = spc.pricePerUnit.currency,
          rules = spc.rules,
          iconUrl = spc.iconUrl
        }

getTicketDef :: Id DTicketPlace.TicketPlace -> Environment.Flow DEM.TicketPlaceDef
getTicketDef placeId = do
  mbDraftChange <- QDTC.findById placeId
  case mbDraftChange >>= (.draftPayload) of
    Just ticketPlaceDef -> return ticketPlaceDef
    Nothing -> do
      ticketDef <- getLiveTicketDef placeId >>= fromMaybeM (InvalidRequest $ "Ticket place not found: " <> getId placeId)
      ticketPlace <- QTicketPlace.findById placeId >>= fromMaybeM (InvalidRequest $ "Ticket place not found: " <> getId placeId)
      now <- getCurrentTime
      let draftTicketChange =
            DDTC.DraftTicketChange
              { id = ticketPlace.id,
                draftPayload = Just ticketDef,
                isApprovalRequired = False,
                status = DDTC.OPEN,
                ticketMerchantId = ticketPlace.ticketMerchantId,
                message = Nothing,
                merchantId = ticketPlace.merchantId,
                merchantOperatingCityId = pure ticketPlace.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QDTC.create draftTicketChange
      return ticketDef

---- draft updations -----
upsertServiceDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.TicketServiceDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
upsertServiceDef ticketPlaceId serviceDef = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedServices =
        if any (\s -> s.id == serviceDef.id) ticketDef.services
          then map (\s -> if s.id == serviceDef.id then serviceDef else s) ticketDef.services
          else ticketDef.services ++ [serviceDef]
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef = ticketDef {DEM.services = updatedServices, DEM.isDraft = not isDraftEqual}
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

delServiceDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
delServiceDef ticketPlaceId serviceId = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedServices = filter (\s -> s.id /= serviceId) ticketDef.services
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef = ticketDef {DEM.services = updatedServices, DEM.isDraft = not isDraftEqual}
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

updateBasicInfo :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.EventManagement.BasicInformation -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
updateBasicInfo ticketPlaceId basicInfo = do
  ticketDef <- getTicketDef ticketPlaceId
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef = ticketDef {DEM.basicInformation = basicInfo, DEM.isDraft = not isDraftEqual}
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId

  -- Invalidate cache when rules are updated
  when (isJust basicInfo.rules) $ do
    invalidateTicketPlaceAvailabilityCache ticketPlaceId

  return updatedTicketDef

upsertServiceCategoryDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Domain.Types.EventManagement.ServiceCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
upsertServiceCategoryDef ticketPlaceId serviceId categoryDef = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedCategories =
        if any (\c -> c.id == categoryDef.id) ticketDef.serviceCategories
          then map (\c -> if c.id == categoryDef.id then categoryDef else c) ticketDef.serviceCategories
          else ticketDef.serviceCategories ++ [categoryDef]
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
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef =
        ticketDef
          { DEM.serviceCategories = updatedCategories,
            DEM.services = updatedServices,
            DEM.isDraft = not isDraftEqual
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
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef =
        ticketDef
          { DEM.serviceCategories = updatedCategories,
            DEM.services = updatedServices,
            DEM.isDraft = not isDraftEqual
          }
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

upsertServicePeopleCategoryDef :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Domain.Types.EventManagement.ServicePeopleCategoryDef -> Environment.Flow Domain.Types.EventManagement.TicketPlaceDef
upsertServicePeopleCategoryDef ticketPlaceId categoryId peopleDef = do
  ticketDef <- getTicketDef ticketPlaceId
  let updatedPeopleCategories =
        if any (\p -> p.id == peopleDef.id) ticketDef.servicePeopleCategories
          then map (\p -> if p.id == peopleDef.id then peopleDef else p) ticketDef.servicePeopleCategories
          else ticketDef.servicePeopleCategories ++ [peopleDef]
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
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef =
        ticketDef
          { DEM.servicePeopleCategories = updatedPeopleCategories,
            DEM.serviceCategories = updatedServiceCategories,
            DEM.isDraft = not isDraftEqual
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
  isDraftEqual <- checkTicketDraftEquality ticketPlaceId
  let updatedTicketDef =
        ticketDef
          { DEM.servicePeopleCategories = updatedPeopleCategories,
            DEM.serviceCategories = updatedServiceCategories,
            DEM.isDraft = not isDraftEqual
          }
  QDTC.updateDraftById (Just updatedTicketDef) False ticketPlaceId
  return updatedTicketDef

checkTicketDraftEquality :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow Bool
checkTicketDraftEquality ticketPlaceId = do
  mbDraftChange <- QDTC.findById ticketPlaceId
  case mbDraftChange >>= (.draftPayload) of
    Just ticketDef -> do
      oldTicketDef <- getLiveTicketDef ticketPlaceId
      return $ maybe False (\oldDef -> compareDef ticketDef oldDef) oldTicketDef
    Nothing -> return True
  where
    compareDef new old =
      new.basicInformation == old.basicInformation
        && old.services == new.services
        && old.serviceCategories == new.serviceCategories
        && old.servicePeopleCategories == new.servicePeopleCategories

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

moveDraftToHistory :: Domain.Types.DraftTicketChange.DraftTicketChange -> Domain.Types.DraftTicketChange.DraftStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow ()
moveDraftToHistory (Domain.Types.DraftTicketChange.DraftTicketChange {..}) status' requestorId = do
  uuid <- generateGUID
  let history =
        DTCH.DraftTicketChangeHistory
          { id = uuid,
            ticketPlaceId = id,
            reviewedBy = requestorId,
            status = status',
            ..
          }
  QDTCH.create history

applyDraftChanges :: Domain.Types.DraftTicketChange.DraftTicketChange -> Environment.Flow ()
applyDraftChanges draftChange = do
  ticketDef <- draftChange.draftPayload & fromMaybeM (InvalidRequest "No draft payload found")
  mocityId <- draftChange.merchantOperatingCityId & fromMaybeM (InvalidRequest "Merchant operating city id is required")
  now <- getCurrentTime
  existingTicketPlace <- QTicketPlace.findById draftChange.id

  -- Invalidate cache when changes with rules are applied
  whenJust ticketDef.basicInformation.rules $ \_ -> do
    invalidateTicketPlaceAvailabilityCache draftChange.id
  let ticketPlaceData =
        DTicketPlace.TicketPlace
          { id = ticketDef.id,
            name = ticketDef.basicInformation.name,
            description = ticketDef.basicInformation.description,
            shortDesc = ticketDef.basicInformation.shortDesc,
            lat = ticketDef.basicInformation.latitude,
            lon = ticketDef.basicInformation.longitude,
            status = ticketDef.basicInformation.status,
            priority = fromMaybe 0 (existingTicketPlace <&> (.priority)),
            placeType = ticketDef.basicInformation.placeType,
            allowSameDayBooking = ticketDef.basicInformation.allowSameDayBooking,
            gallery = ticketDef.basicInformation.gallery,
            iconUrl = ticketDef.basicInformation.iconUrl,
            mapImageUrl = ticketDef.basicInformation.mapImageUrl,
            termsAndConditions = ticketDef.basicInformation.termsAndConditions,
            termsAndConditionsUrl = ticketDef.basicInformation.termsAndConditionsUrl,
            openTimings = ticketDef.basicInformation.openTimings,
            closeTimings = ticketDef.basicInformation.closeTimings,
            customTabs = ticketDef.basicInformation.customTabs,
            ticketMerchantId = draftChange.ticketMerchantId,
            merchantId = fromMaybe (draftChange.merchantId) (existingTicketPlace <&> (.merchantId)),
            merchantOperatingCityId = fromMaybe mocityId (existingTicketPlace <&> (.merchantOperatingCityId)),
            createdAt = now,
            updatedAt = now,
            rules = ticketDef.basicInformation.rules,
            recommend = fromMaybe False (existingTicketPlace <&> (.recommend)),
            faqs = ticketDef.basicInformation.faqs,
            metadata = ticketDef.basicInformation.metadata,
            isRecurring = fromMaybe True ticketDef.basicInformation.isRecurring,
            platformFee = ticketDef.basicInformation.platformFee,
            platformFeeVendor = ticketDef.basicInformation.platformFeeVendor,
            pricingOnwards = ticketDef.basicInformation.pricingOnwards,
            endDate = ticketDef.basicInformation.endDate,
            isClosed = ticketDef.basicInformation.isClosed,
            startDate = ticketDef.basicInformation.startDate,
            venue = ticketDef.basicInformation.venue,
            assignTicketToBpp = ticketDef.basicInformation.assignTicketToBpp,
            enforcedAsSubPlace = ticketDef.basicInformation.enforcedAsSubPlace
          }
  case existingTicketPlace of
    Just extTP -> do
      cleanUpGallery (extTP.merchantOperatingCityId) (extTP.gallery) (ticketDef.basicInformation.gallery)
      QTicketPlace.updateByPrimaryKey ticketPlaceData
    Nothing -> QTicketPlace.create (ticketPlaceData {DTicketPlace.status = DTicketPlace.Unpublished})

  mapM_ processServicePeopleCategory (nubBy (\a b -> a.id == b.id) ticketDef.servicePeopleCategories)

  mapM_ processServiceCategory ticketDef.serviceCategories

  existingBusinessHours <- QBusinessHour.findAllByPlaceId (Just $ draftChange.id.getId)

  usedBhIds <- concatMapM (processTicketService ticketDef) ticketDef.services

  cleanUpBusinessHours (existingBusinessHours <&> (.id)) usedBhIds
  where
    cleanUpGallery :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> [Text] -> [Text] -> Environment.Flow ()
    cleanUpGallery moCityId existingGallery usedGallery = do
      let unusedAssets = existingGallery \\ usedGallery
      forM_ unusedAssets $ \assetUrl -> do
        deleteAsset moCityId $ DTD.DeletePublicFileRequest {DTD.assetId = assetUrl}

    cleanUpBusinessHours :: [Id DBusinessHour.BusinessHour] -> [Id DBusinessHour.BusinessHour] -> Environment.Flow ()
    cleanUpBusinessHours existingBhIds usedBhIds = do
      let unusedBhIds = existingBhIds \\ usedBhIds
      mapM_ (QBusinessHour.deleteById) unusedBhIds

    processServicePeopleCategory :: DEM.ServicePeopleCategoryDef -> Environment.Flow ()
    processServicePeopleCategory spcDef = do
      now <- getCurrentTime
      existingSPC <- QServicePeopleCategory.findByIdAndTimebounds spcDef.id TB.Unbounded
      let servicePC =
            DServicePeopleCategory.ServicePeopleCategory
              { id = spcDef.id,
                name = spcDef.name,
                description = spcDef.description,
                pricingType = spcDef.pricingType,
                pricePerUnit = mkPrice (pure spcDef.priceCurrency) spcDef.priceAmount,
                timeBounds = fromMaybe TB.Unbounded (existingSPC <&> (.timeBounds)),
                vendorSplitDetails = fromMaybe Nothing (existingSPC <&> (.vendorSplitDetails)),
                merchantId = fromMaybe Nothing (existingSPC <&> (.merchantId)),
                merchantOperatingCityId = fromMaybe Nothing (existingSPC <&> (.merchantOperatingCityId)),
                cancellationCharges = fromMaybe Nothing (existingSPC <&> (.cancellationCharges)),
                createdAt = fromMaybe now (existingSPC <&> (.createdAt)),
                placeId = fromMaybe (pure draftChange.id.getId) (existingSPC <&> (.placeId)),
                updatedAt = now,
                rules = spcDef.rules,
                isClosed = fromMaybe False (existingSPC <&> (.isClosed)),
                iconUrl = spcDef.iconUrl
              }
      case existingSPC of
        Just _ -> QServicePeopleCategory.updateByPrimaryKey servicePC
        Nothing -> QServicePeopleCategory.create servicePC

    processServiceCategory :: DEM.ServiceCategoryDef -> Environment.Flow ()
    processServiceCategory scDef = do
      now <- getCurrentTime
      existingSC <- QServiceCategory.findById scDef.id
      let serviceCategory =
            DServiceCategory.ServiceCategory
              { id = scDef.id,
                name = scDef.name,
                description = scDef.description,
                availableSeats = scDef.allowedSeats,
                allowedSeats = scDef.allowedSeats,
                peopleCategory = scDef.peopleCategory,
                placeId = Just draftChange.id.getId,
                merchantId = Nothing,
                merchantOperatingCityId = Nothing,
                createdAt = now,
                updatedAt = now,
                rules = scDef.rules,
                isClosed = fromMaybe False (existingSC <&> (.isClosed)),
                remainingActions = fromMaybe Nothing (existingSC <&> (.remainingActions)),
                inclusionPoints = scDef.inclusionPoints,
                maxSelection = scDef.maxSelection
              }
      case existingSC of
        Just _ -> QServiceCategory.updateByPrimaryKey serviceCategory
        Nothing -> QServiceCategory.create serviceCategory

    groupServiceCategoriesByBusinessHours :: [DEM.ServiceCategoryDef] -> Map DEM.BusinessHourDef [Id DServiceCategory.ServiceCategory]
    groupServiceCategoriesByBusinessHours serviceCategories =
      let initialMap = Map.empty
          updateMapForServiceCategory :: Map DEM.BusinessHourDef [Id DServiceCategory.ServiceCategory] -> DEM.ServiceCategoryDef -> Map DEM.BusinessHourDef [Id DServiceCategory.ServiceCategory]
          updateMapForServiceCategory acc serviceCategory =
            foldl'
              ( \m businessHour ->
                  Map.insertWith (++) businessHour [serviceCategory.id] m
              )
              acc
              serviceCategory.businessHours
       in foldl' updateMapForServiceCategory initialMap serviceCategories

    processBusinessHourGroup :: DEM.TicketPlaceDef -> (DEM.BusinessHourDef, [Id DServiceCategory.ServiceCategory]) -> Environment.Flow (Id DBusinessHour.BusinessHour)
    processBusinessHourGroup ticketDef (businessHour, serviceCategoryIds) = do
      now <- getCurrentTime
      bhId <- generateGUID
      let newBH =
            DBusinessHour.BusinessHour
              { id = bhId,
                btype = businessHour.btype,
                bookingClosingTime = businessHour.bookingClosingTime,
                categoryId = serviceCategoryIds,
                name = Nothing,
                placeId = Just ticketDef.id.getId,
                merchantId = Nothing,
                merchantOperatingCityId = Nothing,
                hash = Nothing,
                expiryDate = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QBusinessHour.create newBH
      return bhId

    processTicketService :: DEM.TicketPlaceDef -> DEM.TicketServiceDef -> Environment.Flow [Id DBusinessHour.BusinessHour]
    processTicketService ticketPlaceDef serviceDef = do
      now <- getCurrentTime
      let ticketPlaceId = ticketPlaceDef.id
      let relatedServiceCategories = filter (\sc -> sc.id `elem` serviceDef.serviceCategoryId) ticketPlaceDef.serviceCategories
      let businessHourMap = groupServiceCategoriesByBusinessHours relatedServiceCategories
      relevantBusinessHourIds <- mapM (processBusinessHourGroup ticketPlaceDef) (Map.toList businessHourMap)
      existingService <- QTicketService.findById serviceDef.id
      let updatedService =
            DTicketService.TicketService
              { id = serviceDef.id,
                service = serviceDef.service,
                shortDesc = serviceDef.shortDesc,
                subPlaceId = serviceDef.subPlaceId,
                operationalDays = serviceDef.operationalDays,
                operationalDate = serviceDef.operationalDate,
                maxVerification = serviceDef.maxVerification,
                allowFutureBooking = serviceDef.allowFutureBooking,
                allowCancellation = serviceDef.allowCancellation,
                expiry = serviceDef.expiry,
                businessHours = relevantBusinessHourIds,
                placesId = ticketPlaceId.getId,
                merchantId = Nothing,
                merchantOperatingCityId = Nothing,
                createdAt = now,
                updatedAt = now,
                rules = existingService >>= (.rules),
                isClosed = fromMaybe False (existingService <&> (.isClosed)),
                serviceDetails = serviceDef.serviceDetails,
                maxSelection = serviceDef.maxSelection,
                note = existingService >>= (.note),
                priority = existingService >>= (.priority)
              }
      case existingService of
        Just _ -> QTicketService.updateByPrimaryKey updatedService
        Nothing -> QTicketService.create updatedService
      return relevantBusinessHourIds

invalidateTicketPlaceAvailabilityCache :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow ()
invalidateTicketPlaceAvailabilityCache placeId = do
  forM_ [1 .. 12] $ \month -> do
    let cacheKey = mkTicketPlaceAvailabilityCacheKey placeId month
    Redis.del cacheKey

  logInfo $ "Invalidated availability cache for place: " <> placeId.getId
  where
    mkTicketPlaceAvailabilityCacheKey :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Int -> Text
    mkTicketPlaceAvailabilityCacheKey placeId' month =
      placeId'.getId <> "<>" <> show month
