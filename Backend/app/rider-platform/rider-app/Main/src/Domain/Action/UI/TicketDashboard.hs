module Domain.Action.UI.TicketDashboard where

import qualified API.Types.Dashboard.AppManagement.Tickets as Tickets
import Control.Applicative ((<|>))
import Control.Monad.Extra (concatMapM)
import Data.List (nubBy)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Action.UI.TicketService as TicketService
import qualified Domain.Types.BusinessHour as DBusinessHour
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOnboarding as MO
import qualified Domain.Types.MerchantOperatingCity as MOCity
import qualified Domain.Types.ServiceCategory as DServiceCategory
import qualified Domain.Types.ServicePeopleCategory as DServicePeopleCategory
import qualified Domain.Types.SpecialOccasion as DSpecialOccasion
import Domain.Types.TicketDashboard
import qualified Domain.Types.TicketPlace as DTicketPlace
import qualified Domain.Types.TicketService as DTicketService
import qualified Domain.Types.TicketSubPlace as DTicketSubPlace
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Flow as Storage
import qualified Storage.Queries.BusinessHour as QBusinessHour
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ServiceCategory as QServiceCategory
import qualified Storage.Queries.ServicePeopleCategory as QServicePeopleCategory
import qualified Storage.Queries.SpecialOccasion as QSpecialOccasion
import qualified Storage.Queries.TicketMerchantDetails as QTMD
import qualified Storage.Queries.TicketPlace as QTicketPlace
import qualified Storage.Queries.TicketService as QTicketService
import qualified Storage.Queries.TicketSubPlace as QTicketSubPlace
import Tools.Error

getTicketDashboardUserInfo :: Text -> MO.RequestorRole -> Environment.Flow Tickets.TicketDashboardUserInfo
getTicketDashboardUserInfo userId userRole = do
  person <- QP.findById (Id userId) >>= fromMaybeM (PersonNotFound userId)
  mbTicketMerchantDetails <- QTMD.findById (Id userId)
  mobileNumber <- case person.mobileNumber of
    Just encMobNum -> decrypt encMobNum
    Nothing -> throwError $ InternalError "Mobile number not found"

  let mbDecryptedFields = case mbTicketMerchantDetails of
        Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
        Just details -> do
          mbAgreementLetter <- mapM decrypt details.agreementLetter
          bankAccountNumber <- decrypt details.bankAccountNumber
          bankIfsc <- decrypt details.bankIfsc
          mbDocCancelledCheque <- mapM decrypt details.docCancelledCheque
          docPan <- decrypt details.docPan
          mbGstin <- mapM decrypt details.gstin
          pan <- decrypt details.pan
          pure (mbAgreementLetter, Just bankAccountNumber, Just bankIfsc, mbDocCancelledCheque, Just docPan, mbGstin, Just pan)

  (mbDecryptedAgreementLetter, mbDecryptedBankAccountNumber, mbDecryptedBankIfsc, mbDecryptedDocCancelledCheque, mbDecryptedDocPan, mbDecryptedGstin, mbDecryptedPan) <- mbDecryptedFields

  return
    Tickets.TicketDashboardUserInfo
      { firstName = person.firstName,
        lastName = person.lastName,
        role = userRole,
        registeredNumber = mobileNumber,
        agreementLetter = mbDecryptedAgreementLetter,
        bankAccountNumber = mbDecryptedBankAccountNumber,
        bankAccountType = mbTicketMerchantDetails >>= \details -> Just details.bankAccountType,
        bankBeneficiaryName = mbTicketMerchantDetails >>= \details -> Just details.bankBeneficiaryName,
        bankIfsc = mbDecryptedBankIfsc,
        contactDetails = mbTicketMerchantDetails >>= \details -> Just details.contactDetails,
        docCancelledCheque = mbDecryptedDocCancelledCheque,
        docPan = mbDecryptedDocPan,
        gstin = mbDecryptedGstin,
        orgAddress = mbTicketMerchantDetails >>= (.orgAddress),
        orgName = mbTicketMerchantDetails >>= \details -> Just details.orgName,
        pan = mbDecryptedPan,
        state = mbTicketMerchantDetails >>= \details -> Just details.state
      }

getTicketDashboardFile :: Text -> Environment.Flow MO.GetFileResponse
getTicketDashboardFile fileId = do
  file <- MFQuery.findById (Id fileId) >>= fromMaybeM (InvalidRequest "No file found")
  filePath <- file.s3FilePath & fromMaybeM (FileDoNotExist fileId)
  base64File <- Storage.get $ T.unpack filePath
  return $
    MO.GetFileResponse
      { fileBase64 = base64File,
        fileType = show file._type
      }

getTicketPlaceDashboardDetails :: Id DTicketPlace.TicketPlace -> Maybe Text -> Maybe MO.RequestorRole -> Environment.Flow TicketPlaceDashboardDetails
getTicketPlaceDashboardDetails placeId _requestorId _requestorRole = do
  ticketPlace <- QTicketPlace.findById placeId >>= fromMaybeM (InvalidRequest $ "Ticket place not found: " <> getId placeId)

  services <- QTicketService.getTicketServicesByPlaceId placeId.getId

  let linkedBusinessHourIds = concatMap (.businessHours) services
  linkedBusinessHours <- catMaybes <$> mapM QBusinessHour.findById linkedBusinessHourIds
  unlinkedBusinessHours <- QBusinessHour.findAllByPlaceId (Just placeId.getId)
  let allBusinessHoursExceptSpecialOccasions = nubBy (\h1 h2 -> h1.id == h2.id) $ linkedBusinessHours ++ unlinkedBusinessHours

  let linkedServiceCategoryIds = concatMap (.categoryId) allBusinessHoursExceptSpecialOccasions
  linkedServiceCategories <- catMaybes <$> mapM QServiceCategory.findById linkedServiceCategoryIds
  unlinkedServiceCategories <- QServiceCategory.findAllByPlaceId (Just placeId.getId)
  let allServiceCategoriesExceptSpecialOccasions = nubBy (\h1 h2 -> h1.id == h2.id) $ linkedServiceCategories ++ unlinkedServiceCategories

  linkedSpecialOcassion <- concatMapM QSpecialOccasion.findAllByEntityId (((.id.getId) <$> allServiceCategoriesExceptSpecialOccasions) ++ ((.id.getId) <$> services))
  unlinkedSpecialOccasions <- QSpecialOccasion.findAllByPlaceId (Just placeId.getId)
  let allSpecialOccasions = nubBy (\h1 h2 -> h1.id == h2.id) $ linkedSpecialOcassion ++ unlinkedSpecialOccasions

  let alreadyFoundBusinessHourIds = allBusinessHoursExceptSpecialOccasions <&> (.id)
      allSpecialOccasionBHourIds = concatMap (.businessHours) allSpecialOccasions
      allRemainingBusinessHourIds = filter (\bh -> not $ elem bh alreadyFoundBusinessHourIds) allSpecialOccasionBHourIds
  allRemainingBusinessHour <- catMaybes <$> mapM QBusinessHour.findById allRemainingBusinessHourIds
  let allBusinessHours = nubBy (\h1 h2 -> h1.id == h2.id) $ allBusinessHoursExceptSpecialOccasions ++ allRemainingBusinessHour

  let alreadyFoundServiceCategoryIds = allServiceCategoriesExceptSpecialOccasions <&> (.id)
      remainingServiceCategoryIds = filter (\sc -> not $ elem sc alreadyFoundServiceCategoryIds) (concatMap (.categoryId) allRemainingBusinessHour)
  allRemainingServiceCategories <- catMaybes <$> mapM QServiceCategory.findById remainingServiceCategoryIds
  let allServiceCategories = nubBy (\h1 h2 -> h1.id == h2.id) $ allServiceCategoriesExceptSpecialOccasions ++ allRemainingServiceCategories

  let linkedServicePeopleCategoryIds = concatMap (.peopleCategory) allServiceCategories
  linkedServicePeopleCategories <- concatMapM QServicePeopleCategory.findAllById linkedServicePeopleCategoryIds
  unlinkedServicePeopleCategories <- QServicePeopleCategory.findAllByPlaceId (Just placeId.getId)
  let allServicePeopleCategories = nubBy (\h1 h2 -> h1.id == h2.id && h1.timeBounds == h2.timeBounds) $ linkedServicePeopleCategories ++ unlinkedServicePeopleCategories

  let serviceDetails = map toTicketServiceDetails services
      businessHourDetails = map toBusinessHourDetails allBusinessHours
      serviceCategoryDetails = map toServiceCategoryDetails allServiceCategories
      servicePeopleCategoryDetails = map toServicePeopleCategoryDetails allServicePeopleCategories
      specialOccasionDetails = map toSpecialOccasionDetails allSpecialOccasions

  return
    TicketPlaceDashboardDetails
      { id = ticketPlace.id,
        name = ticketPlace.name,
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
        services = serviceDetails,
        businessHours = businessHourDetails,
        serviceCategories = serviceCategoryDetails,
        servicePeopleCategories = servicePeopleCategoryDetails,
        specialOccasions = specialOccasionDetails,
        faqs = ticketPlace.faqs,
        isRecurring = Just ticketPlace.isRecurring,
        metadata = ticketPlace.metadata,
        platformFee = ticketPlace.platformFee,
        platformFeeVendor = ticketPlace.platformFeeVendor,
        pricingOnwards = ticketPlace.pricingOnwards,
        startDate = ticketPlace.startDate,
        endDate = ticketPlace.endDate,
        venue = ticketPlace.venue,
        rules = ticketPlace.rules,
        assignTicketToBpp = Just ticketPlace.assignTicketToBpp,
        customTabs = ticketPlace.customTabs,
        recommend = Just ticketPlace.recommend,
        enforcedAsSubPlace = Just ticketPlace.enforcedAsSubPlace,
        merchantOperatingCityId = Just ticketPlace.merchantOperatingCityId.getId
      }
  where
    toTicketServiceDetails :: DTicketService.TicketService -> TicketServiceDetails
    toTicketServiceDetails svc =
      TicketServiceDetails
        { id = svc.id,
          service = svc.service,
          shortDesc = svc.shortDesc,
          operationalDays = svc.operationalDays,
          operationalDate = svc.operationalDate,
          maxVerification = svc.maxVerification,
          allowFutureBooking = svc.allowFutureBooking,
          allowCancellation = svc.allowCancellation,
          expiry = svc.expiry,
          businessHours = svc.businessHours,
          rules = svc.rules,
          serviceDetails = svc.serviceDetails,
          subPlaceId = svc.subPlaceId,
          maxSelection = svc.maxSelection,
          note = svc.note,
          priority = svc.priority
        }

    toBusinessHourDetails :: DBusinessHour.BusinessHour -> BusinessHourDetails
    toBusinessHourDetails bh =
      BusinessHourDetails
        { id = bh.id,
          name = fromMaybe bh.id.getId bh.name,
          btype = bh.btype,
          categoryId = bh.categoryId,
          bookingClosingTime = bh.bookingClosingTime
        }

    toServiceCategoryDetails :: DServiceCategory.ServiceCategory -> ServiceCategoryDetails
    toServiceCategoryDetails sc =
      ServiceCategoryDetails
        { id = sc.id,
          name = sc.name,
          description = sc.description,
          allowedSeats = sc.allowedSeats,
          availableSeats = sc.availableSeats,
          peopleCategory = sc.peopleCategory,
          inclusionPoints = sc.inclusionPoints,
          rules = sc.rules,
          maxSelection = sc.maxSelection
        }

    toServicePeopleCategoryDetails :: DServicePeopleCategory.ServicePeopleCategory -> ServicePeopleCategoryDetails
    toServicePeopleCategoryDetails spc =
      ServicePeopleCategoryDetails
        { id = spc.id,
          name = spc.name,
          description = spc.description,
          pricingType = spc.pricingType,
          priceAmount = spc.pricePerUnit.amount,
          priceCurrency = spc.pricePerUnit.currency,
          timeBounds = spc.timeBounds,
          vendorSplitDetails = spc.vendorSplitDetails,
          rules = spc.rules,
          iconUrl = spc.iconUrl
        }

    toSpecialOccasionDetails :: DSpecialOccasion.SpecialOccasion -> SpecialOccasionDetails
    toSpecialOccasionDetails so =
      SpecialOccasionDetails
        { name = fromMaybe so.id.getId so.name,
          placeId = so.placeId,
          id = so.id,
          entityId = so.entityId,
          date = so.date,
          dayOfWeek = so.dayOfWeek,
          specialDayType = so.specialDayType,
          description = so.description,
          businessHours = so.businessHours
        }

-- Helper functions for updating and creating entities
updateTicketPlace :: DTicketPlace.TicketPlace -> TicketPlaceDashboardDetails -> DTicketPlace.TicketPlace
updateTicketPlace existingPlace placeDetails = do
  existingPlace
    { DTicketPlace.name = placeDetails.name,
      DTicketPlace.description = placeDetails.description,
      DTicketPlace.shortDesc = placeDetails.shortDesc,
      DTicketPlace.lat = placeDetails.latitude,
      DTicketPlace.lon = placeDetails.longitude,
      DTicketPlace.status = placeDetails.status,
      DTicketPlace.priority = placeDetails.priority,
      DTicketPlace.placeType = placeDetails.placeType,
      DTicketPlace.allowSameDayBooking = placeDetails.allowSameDayBooking,
      DTicketPlace.gallery = placeDetails.gallery,
      DTicketPlace.iconUrl = placeDetails.iconUrl,
      DTicketPlace.mapImageUrl = placeDetails.mapImageUrl,
      DTicketPlace.termsAndConditions = placeDetails.termsAndConditions,
      DTicketPlace.termsAndConditionsUrl = placeDetails.termsAndConditionsUrl,
      DTicketPlace.openTimings = placeDetails.openTimings,
      DTicketPlace.closeTimings = placeDetails.closeTimings,
      DTicketPlace.rules = placeDetails.rules,
      DTicketPlace.startDate = placeDetails.startDate,
      DTicketPlace.endDate = placeDetails.endDate,
      DTicketPlace.venue = placeDetails.venue,
      DTicketPlace.assignTicketToBpp = fromMaybe existingPlace.assignTicketToBpp placeDetails.assignTicketToBpp,
      DTicketPlace.faqs = placeDetails.faqs,
      DTicketPlace.metadata = placeDetails.metadata,
      DTicketPlace.isRecurring = fromMaybe True placeDetails.isRecurring,
      DTicketPlace.platformFee = placeDetails.platformFee,
      DTicketPlace.platformFeeVendor = placeDetails.platformFeeVendor,
      DTicketPlace.pricingOnwards = placeDetails.pricingOnwards,
      DTicketPlace.customTabs = placeDetails.customTabs <|> existingPlace.customTabs,
      DTicketPlace.recommend = fromMaybe existingPlace.recommend placeDetails.recommend,
      DTicketPlace.enforcedAsSubPlace = fromMaybe existingPlace.enforcedAsSubPlace placeDetails.enforcedAsSubPlace
    }

createTicketPlace :: TicketPlaceDashboardDetails -> Maybe Text -> Id Merchant.Merchant -> Id MOCity.MerchantOperatingCity -> Environment.Flow DTicketPlace.TicketPlace
createTicketPlace placeDetails creatorId merchantId merchantOpCityId = do
  now <- getCurrentTime
  pure $
    DTicketPlace.TicketPlace
      { DTicketPlace.id = placeDetails.id,
        DTicketPlace.name = placeDetails.name,
        DTicketPlace.description = placeDetails.description,
        DTicketPlace.shortDesc = placeDetails.shortDesc,
        DTicketPlace.lat = placeDetails.latitude,
        DTicketPlace.lon = placeDetails.longitude,
        DTicketPlace.status = placeDetails.status,
        DTicketPlace.priority = placeDetails.priority,
        DTicketPlace.placeType = placeDetails.placeType,
        DTicketPlace.allowSameDayBooking = placeDetails.allowSameDayBooking,
        DTicketPlace.gallery = placeDetails.gallery,
        DTicketPlace.iconUrl = placeDetails.iconUrl,
        DTicketPlace.mapImageUrl = placeDetails.mapImageUrl,
        DTicketPlace.termsAndConditions = placeDetails.termsAndConditions,
        DTicketPlace.termsAndConditionsUrl = placeDetails.termsAndConditionsUrl,
        DTicketPlace.openTimings = placeDetails.openTimings,
        DTicketPlace.closeTimings = placeDetails.closeTimings,
        DTicketPlace.merchantId = pure merchantId,
        DTicketPlace.merchantOperatingCityId = merchantOpCityId,
        DTicketPlace.ticketMerchantId = creatorId,
        DTicketPlace.customTabs = Nothing,
        DTicketPlace.createdAt = now,
        DTicketPlace.updatedAt = now,
        DTicketPlace.rules = Nothing,
        DTicketPlace.recommend = False,
        DTicketPlace.faqs = placeDetails.faqs,
        DTicketPlace.isRecurring = fromMaybe True placeDetails.isRecurring,
        DTicketPlace.metadata = placeDetails.metadata,
        DTicketPlace.platformFee = placeDetails.platformFee,
        DTicketPlace.platformFeeVendor = placeDetails.platformFeeVendor,
        DTicketPlace.pricingOnwards = placeDetails.pricingOnwards,
        DTicketPlace.endDate = placeDetails.endDate,
        DTicketPlace.isClosed = False,
        DTicketPlace.startDate = placeDetails.startDate,
        DTicketPlace.venue = placeDetails.venue,
        DTicketPlace.assignTicketToBpp = fromMaybe False placeDetails.assignTicketToBpp,
        DTicketPlace.enforcedAsSubPlace = False
      }

updateTicketService :: DTicketService.TicketService -> TicketServiceDetails -> DTicketService.TicketService
updateTicketService existingService serviceDetails = do
  existingService
    { DTicketService.service = serviceDetails.service,
      DTicketService.shortDesc = serviceDetails.shortDesc,
      DTicketService.operationalDays = serviceDetails.operationalDays,
      DTicketService.operationalDate = serviceDetails.operationalDate,
      DTicketService.maxVerification = serviceDetails.maxVerification,
      DTicketService.allowFutureBooking = serviceDetails.allowFutureBooking,
      DTicketService.expiry = serviceDetails.expiry,
      DTicketService.allowCancellation = serviceDetails.allowCancellation,
      DTicketService.businessHours = serviceDetails.businessHours,
      DTicketService.rules = serviceDetails.rules,
      DTicketService.serviceDetails = serviceDetails.serviceDetails,
      DTicketService.maxSelection = serviceDetails.maxSelection,
      DTicketService.note = serviceDetails.note,
      DTicketService.priority = serviceDetails.priority
    }

createTicketService :: (Id Merchant.Merchant, Id MOCity.MerchantOperatingCity) -> TicketServiceDetails -> Id DTicketPlace.TicketPlace -> Flow DTicketService.TicketService
createTicketService (merchantId, merchantOpCityId) serviceDetails placeId = do
  now <- getCurrentTime
  pure $
    DTicketService.TicketService
      { DTicketService.id = serviceDetails.id,
        DTicketService.service = serviceDetails.service,
        DTicketService.shortDesc = serviceDetails.shortDesc,
        DTicketService.subPlaceId = serviceDetails.subPlaceId,
        DTicketService.operationalDays = serviceDetails.operationalDays,
        DTicketService.operationalDate = serviceDetails.operationalDate,
        DTicketService.maxVerification = serviceDetails.maxVerification,
        DTicketService.allowFutureBooking = serviceDetails.allowFutureBooking,
        DTicketService.expiry = serviceDetails.expiry,
        DTicketService.allowCancellation = serviceDetails.allowCancellation,
        DTicketService.businessHours = serviceDetails.businessHours,
        DTicketService.placesId = placeId.getId,
        DTicketService.merchantId = pure merchantId,
        DTicketService.merchantOperatingCityId = pure merchantOpCityId,
        DTicketService.createdAt = now,
        DTicketService.updatedAt = now,
        DTicketService.note = Nothing,
        DTicketService.priority = Nothing,
        DTicketService.rules = Nothing,
        DTicketService.isClosed = False,
        DTicketService.serviceDetails = serviceDetails.serviceDetails,
        DTicketService.maxSelection = serviceDetails.maxSelection
      }

updateBusinessHour :: DBusinessHour.BusinessHour -> BusinessHourDetails -> DBusinessHour.BusinessHour
updateBusinessHour existingBH bhDetails = do
  existingBH
    { DBusinessHour.name = pure bhDetails.name,
      DBusinessHour.btype = bhDetails.btype,
      DBusinessHour.categoryId = bhDetails.categoryId,
      DBusinessHour.bookingClosingTime = bhDetails.bookingClosingTime
    }

createBusinessHour :: (Id Merchant.Merchant, Id MOCity.MerchantOperatingCity) -> BusinessHourDetails -> Id DTicketPlace.TicketPlace -> Environment.Flow DBusinessHour.BusinessHour
createBusinessHour (merchantId, merchantOpCityId) bhDetails placeId = do
  now <- getCurrentTime
  pure $
    DBusinessHour.BusinessHour
      { DBusinessHour.id = bhDetails.id,
        DBusinessHour.name = pure bhDetails.name,
        DBusinessHour.btype = bhDetails.btype,
        DBusinessHour.categoryId = bhDetails.categoryId,
        DBusinessHour.bookingClosingTime = bhDetails.bookingClosingTime,
        DBusinessHour.placeId = pure placeId.getId,
        DBusinessHour.merchantId = pure merchantId,
        DBusinessHour.merchantOperatingCityId = pure merchantOpCityId,
        DBusinessHour.hash = Nothing,
        DBusinessHour.expiryDate = Nothing,
        DBusinessHour.createdAt = now,
        DBusinessHour.updatedAt = now
      }

updateServiceCategory :: DServiceCategory.ServiceCategory -> ServiceCategoryDetails -> DServiceCategory.ServiceCategory
updateServiceCategory existingSC scDetails = do
  existingSC
    { DServiceCategory.name = scDetails.name,
      DServiceCategory.description = scDetails.description,
      DServiceCategory.availableSeats = scDetails.availableSeats,
      DServiceCategory.allowedSeats = scDetails.allowedSeats,
      DServiceCategory.peopleCategory = scDetails.peopleCategory,
      DServiceCategory.rules = scDetails.rules,
      DServiceCategory.inclusionPoints = scDetails.inclusionPoints,
      DServiceCategory.maxSelection = scDetails.maxSelection
    }

createServiceCategory :: (Id Merchant.Merchant, Id MOCity.MerchantOperatingCity) -> ServiceCategoryDetails -> Id DTicketPlace.TicketPlace -> Environment.Flow DServiceCategory.ServiceCategory
createServiceCategory (merchantId, merchantOpCityId) scDetails placeId = do
  now <- getCurrentTime
  pure $
    DServiceCategory.ServiceCategory
      { DServiceCategory.id = scDetails.id,
        DServiceCategory.name = scDetails.name,
        DServiceCategory.description = scDetails.description,
        DServiceCategory.availableSeats = scDetails.availableSeats,
        DServiceCategory.allowedSeats = scDetails.allowedSeats,
        DServiceCategory.peopleCategory = scDetails.peopleCategory,
        DServiceCategory.placeId = pure placeId.getId,
        DServiceCategory.merchantId = pure merchantId,
        DServiceCategory.merchantOperatingCityId = pure merchantOpCityId,
        DServiceCategory.createdAt = now,
        DServiceCategory.updatedAt = now,
        DServiceCategory.rules = Nothing,
        DServiceCategory.isClosed = False,
        DServiceCategory.remainingActions = Nothing,
        DServiceCategory.inclusionPoints = scDetails.inclusionPoints,
        DServiceCategory.maxSelection = scDetails.maxSelection
      }

updateServicePeopleCategory :: DServicePeopleCategory.ServicePeopleCategory -> ServicePeopleCategoryDetails -> DServicePeopleCategory.ServicePeopleCategory
updateServicePeopleCategory existingSPC spcDetails = do
  existingSPC
    { DServicePeopleCategory.name = spcDetails.name,
      DServicePeopleCategory.description = spcDetails.description,
      DServicePeopleCategory.pricingType = spcDetails.pricingType,
      DServicePeopleCategory.pricePerUnit = mkPrice (pure spcDetails.priceCurrency) spcDetails.priceAmount,
      DServicePeopleCategory.timeBounds = spcDetails.timeBounds,
      DServicePeopleCategory.vendorSplitDetails = spcDetails.vendorSplitDetails,
      DServicePeopleCategory.rules = spcDetails.rules,
      DServicePeopleCategory.iconUrl = spcDetails.iconUrl
    }

createServicePeopleCategory :: (Id Merchant.Merchant, Id MOCity.MerchantOperatingCity) -> ServicePeopleCategoryDetails -> Id DTicketPlace.TicketPlace -> Environment.Flow DServicePeopleCategory.ServicePeopleCategory
createServicePeopleCategory (merchantId, merchantOpCityId) spcDetails placeId = do
  now <- getCurrentTime
  pure $
    DServicePeopleCategory.ServicePeopleCategory
      { DServicePeopleCategory.id = spcDetails.id,
        DServicePeopleCategory.name = spcDetails.name,
        DServicePeopleCategory.description = spcDetails.description,
        DServicePeopleCategory.pricingType = spcDetails.pricingType,
        DServicePeopleCategory.pricePerUnit = mkPrice (pure spcDetails.priceCurrency) spcDetails.priceAmount,
        DServicePeopleCategory.timeBounds = spcDetails.timeBounds,
        DServicePeopleCategory.vendorSplitDetails = spcDetails.vendorSplitDetails,
        DServicePeopleCategory.placeId = pure placeId.getId,
        DServicePeopleCategory.merchantId = pure merchantId,
        DServicePeopleCategory.merchantOperatingCityId = pure merchantOpCityId,
        DServicePeopleCategory.createdAt = now,
        DServicePeopleCategory.updatedAt = now,
        DServicePeopleCategory.cancellationCharges = Nothing,
        DServicePeopleCategory.rules = Nothing,
        DServicePeopleCategory.isClosed = False,
        DServicePeopleCategory.iconUrl = Nothing
      }

updateSpecialOccasion :: DSpecialOccasion.SpecialOccasion -> SpecialOccasionDetails -> DSpecialOccasion.SpecialOccasion
updateSpecialOccasion existingSO soDetails = do
  existingSO
    { DSpecialOccasion.name = Just soDetails.name,
      DSpecialOccasion.entityId = soDetails.entityId,
      DSpecialOccasion.date = soDetails.date,
      DSpecialOccasion.dayOfWeek = soDetails.dayOfWeek,
      DSpecialOccasion.specialDayType = soDetails.specialDayType,
      DSpecialOccasion.description = soDetails.description,
      DSpecialOccasion.businessHours = soDetails.businessHours
    }

createSpecialOccasion :: (Id Merchant.Merchant, Id MOCity.MerchantOperatingCity) -> SpecialOccasionDetails -> Id DTicketPlace.TicketPlace -> Environment.Flow DSpecialOccasion.SpecialOccasion
createSpecialOccasion (merchantId, merchantOpCityId) soDetails placeId = do
  now <- getCurrentTime
  pure $
    DSpecialOccasion.SpecialOccasion
      { DSpecialOccasion.id = soDetails.id,
        DSpecialOccasion.name = Just soDetails.name,
        DSpecialOccasion.entityId = soDetails.entityId,
        DSpecialOccasion.date = soDetails.date,
        DSpecialOccasion.dayOfWeek = soDetails.dayOfWeek,
        DSpecialOccasion.specialDayType = soDetails.specialDayType,
        DSpecialOccasion.description = soDetails.description,
        DSpecialOccasion.businessHours = soDetails.businessHours,
        DSpecialOccasion.placeId = pure placeId.getId,
        DSpecialOccasion.merchantId = pure merchantId,
        DSpecialOccasion.merchantOperatingCityId = pure merchantOpCityId,
        DSpecialOccasion.createdAt = now,
        DSpecialOccasion.updatedAt = now
      }

postUpsertTicketPlaceDashboardDetails :: (Id Merchant.Merchant, Id MOCity.MerchantOperatingCity) -> TicketPlaceDashboardDetails -> Maybe Text -> Maybe MO.RequestorRole -> Environment.Flow ()
postUpsertTicketPlaceDashboardDetails (merchantId, merchantOpCityId) placeDetails requestorId requestorRole = do
  when (T.null placeDetails.name) $ throwError $ InvalidRequest "Ticket place name cannot be empty"

  -- Check if the ticket place exists
  mbExistingPlace <- QTicketPlace.findById placeDetails.id

  -- Update or create the ticket place
  ticketPlace <- case mbExistingPlace of
    Just existingPlace -> do
      let updatedPlace = updateTicketPlace existingPlace placeDetails
      QTicketPlace.updateByPrimaryKey updatedPlace
      -- Invalidate cache when ticket place is updated
      TicketService.invalidateCacheForTicketPlace updatedPlace.id
      return updatedPlace
    Nothing -> do
      -- Create new place
      let creatorId = if requestorRole == Just MO.TICKET_DASHBOARD_ADMIN then Nothing else requestorId
      newPlace <- createTicketPlace placeDetails creatorId merchantId (maybe merchantOpCityId Id placeDetails.merchantOperatingCityId)
      QTicketPlace.create newPlace
      return newPlace

  -- Update or create services
  forM_ placeDetails.services $ \serviceDetails -> do
    mbExistingService <- QTicketService.findById serviceDetails.id
    case mbExistingService of
      Just existingService -> do
        -- Update existing service
        let updatedService = updateTicketService existingService serviceDetails
        QTicketService.updateByPrimaryKey updatedService
      Nothing -> do
        -- Create new service
        newService <- createTicketService (merchantId, merchantOpCityId) serviceDetails ticketPlace.id
        QTicketService.create newService

  -- Update or create business hours
  forM_ placeDetails.businessHours $ \bhDetails -> do
    mbExistingBH <- QBusinessHour.findById bhDetails.id
    case mbExistingBH of
      Just existingBH -> do
        -- Update existing business hour
        let updatedBH = updateBusinessHour existingBH bhDetails
        QBusinessHour.updateByPrimaryKey updatedBH
      Nothing -> do
        -- Create new business hour
        newBH <- createBusinessHour (merchantId, merchantOpCityId) bhDetails ticketPlace.id
        QBusinessHour.create newBH

  -- Update or create service categories
  forM_ placeDetails.serviceCategories $ \scDetails -> do
    mbExistingSC <- QServiceCategory.findById scDetails.id
    case mbExistingSC of
      Just existingSC -> do
        -- Update existing service category
        let updatedSC = updateServiceCategory existingSC scDetails
        QServiceCategory.updateByPrimaryKey updatedSC
      Nothing -> do
        -- Create new service category
        newSC <- createServiceCategory (merchantId, merchantOpCityId) scDetails ticketPlace.id
        QServiceCategory.create newSC

  -- Update or create service people categories
  let allIncomingSPCIds = concatMap (.peopleCategory) placeDetails.serviceCategories
  existingSPCs <- concat <$> mapM QServicePeopleCategory.findAllById allIncomingSPCIds
  let idCountMap = foldl' (\acc spc -> Map.insertWith (+) spc.id 1 acc) Map.empty existingSPCs
  let spcsToDelete =
        filter
          ( \existingSPC ->
              not $
                any
                  ( \incomingSPC ->
                      incomingSPC.id == existingSPC.id && incomingSPC.timeBounds == existingSPC.timeBounds
                  )
                  placeDetails.servicePeopleCategories
          )
          existingSPCs
  let finalIdCountMap = foldl' (\acc spc -> Map.insertWith (+) spc.id (-1) acc) idCountMap spcsToDelete
  forM_ spcsToDelete $ \spc -> do
    let currentCount = fromMaybe 0 $ Map.lookup spc.id finalIdCountMap :: Int
    when (currentCount > 0) $
      QServicePeopleCategory.deleteByIdAndTimebounds spc.id spc.timeBounds

  forM_ placeDetails.servicePeopleCategories $ \spcDetails -> do
    mbExistingSPC <- QServicePeopleCategory.findByIdAndTimebounds spcDetails.id spcDetails.timeBounds
    case mbExistingSPC of
      Just existingSPC -> do
        let updatedSPC = updateServicePeopleCategory existingSPC spcDetails
        QServicePeopleCategory.updateByIdAndTimebounds updatedSPC
      Nothing -> do
        newSPC <- createServicePeopleCategory (merchantId, merchantOpCityId) spcDetails ticketPlace.id
        QServicePeopleCategory.create newSPC

  -- Update or create special occasions
  forM_ placeDetails.specialOccasions $ \soDetails -> do
    -- For special occasions, we need to find by primary key
    mbExistingSO <- QSpecialOccasion.findByPrimaryKey soDetails.id
    case mbExistingSO of
      Just existingSO -> do
        -- Update existing special occasion
        let updatedSO = updateSpecialOccasion existingSO soDetails
        QSpecialOccasion.updateByPrimaryKey updatedSO
      Nothing -> do
        -- Create new special occasion
        newSO <- createSpecialOccasion (merchantId, merchantOpCityId) soDetails ticketPlace.id
        QSpecialOccasion.create newSO

getTicketPlaceDashboardList :: Text -> Maybe Text -> Maybe MO.RequestorRole -> Environment.Flow [DTicketPlace.TicketPlace]
getTicketPlaceDashboardList status _requestorId _requestorRole = do
  placeStatus <- fromMaybeM (InvalidRequest "Invalid status query param") $ readMaybe (T.unpack status)
  QTicketPlace.getAllTicketPlaces placeStatus

getTicketPlaceDashboardSubPlaces :: Id DTicketPlace.TicketPlace -> Environment.Flow [DTicketSubPlace.TicketSubPlace]
getTicketPlaceDashboardSubPlaces placeId = do
  QTicketSubPlace.findAllByTicketPlaceId placeId

postUpsertTicketPlaceDashboardSubPlaces :: Id DTicketPlace.TicketPlace -> [DTicketSubPlace.TicketSubPlace] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postUpsertTicketPlaceDashboardSubPlaces _ subPlaces = do
  forM_ subPlaces $ \subPlace -> do
    mbExistingSubPlace <- QTicketSubPlace.findById subPlace.id
    case mbExistingSubPlace of
      Just _ -> QTicketSubPlace.updateByPrimaryKey subPlace
      Nothing -> QTicketSubPlace.create subPlace
  return Kernel.Types.APISuccess.Success
