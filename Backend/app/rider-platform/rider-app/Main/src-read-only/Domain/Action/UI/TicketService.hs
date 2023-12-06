{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.TicketService where

import qualified Data.List.NonEmpty as DLN
import Data.OpenApi (ToSchema)
import qualified Data.Text as Data.Text
import Data.Time (UTCTime (UTCTime), dayOfWeek, midnight, timeOfDayToTime, utctDay)
import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Action.UI.Tickets as Domain.Action.UI.Tickets
import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.SeatManagement as Domain.Types.SeatManagement
import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory as Domain.Types.ServicePeopleCategory
import qualified Domain.Types.SpecialOccasion as Domain.Types.SpecialOccasion
import qualified Domain.Types.TicketBooking as DTTB
import qualified Domain.Types.TicketBooking as Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingPeopleCategory as DTB
import qualified Domain.Types.TicketBookingService as DTB
import qualified Domain.Types.TicketBookingService as Domain.Types.TicketBookingService
import qualified Domain.Types.TicketBookingServiceCategory as DTB
import qualified Domain.Types.TicketPlace as Domain.Types.TicketPlace
import qualified Domain.Types.TicketService as Domain.Types.TicketService
import qualified Domain.Types.Tickets as Domain.Types.Tickets
import qualified Environment as Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.APISuccess as Kernel.Types.APISuccess
import qualified Kernel.Types.Common as Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.BusinessHour as QBH
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SeatManagement as QTSM
import qualified Storage.Queries.ServiceCategory as QSC
import qualified Storage.Queries.ServicePeopleCategory as QPC
import qualified Storage.Queries.SpecialOccasion as QSO
import qualified Storage.Queries.TicketBooking as QTB
import qualified Storage.Queries.TicketBookingPeopleCategory as QTBPC
import qualified Storage.Queries.TicketBookingService as QTBS
import qualified Storage.Queries.TicketBookingServiceCategory as QTBSC
import qualified Storage.Queries.TicketPlace as QTP
import qualified Storage.Queries.TicketService as QTS
import Tools.Auth
import Tools.Error
import qualified Tools.Payment as Payment

data BusinessHourResp = BusinessHourResp
  { categories :: [Domain.Action.UI.TicketService.CategoriesResp],
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    id :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    operationalDays :: [Data.Text.Text],
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    specialDayDescription :: Kernel.Prelude.Maybe Data.Text.Text,
    specialDayType :: Kernel.Prelude.Maybe Domain.Types.SpecialOccasion.SpecialDayType,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CategoriesResp = CategoriesResp
  { allowedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookedSeats :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    name :: Data.Text.Text,
    peopleCategories :: [Domain.Action.UI.TicketService.PeopleCategoriesResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PeopleCategoriesResp = PeopleCategoriesResp
  { description :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Data.Text.Text,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PeopleCategoriesVerificationRes = PeopleCategoriesVerificationRes
  { id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Data.Text.Text,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingAPIEntity = TicketBookingAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    personId :: Data.Text.Text,
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Data.Text.Text,
    ticketPlaceName :: Data.Text.Text,
    ticketShortId :: Data.Text.Text,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingCategoryDetails = TicketBookingCategoryDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: Kernel.Prelude.Int,
    name :: Data.Text.Text,
    peopleCategories :: [Domain.Action.UI.TicketService.TicketBookingPeopleCategoryDetails]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingCategoryReq = TicketBookingCategoryReq
  { categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    peopleCategories :: [Domain.Action.UI.TicketService.TicketBookingPeopleCategoryReq]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingDetails = TicketBookingDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    personId :: Data.Text.Text,
    services :: [Domain.Action.UI.TicketService.TicketBookingServiceDetails],
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Data.Text.Text,
    ticketPlaceName :: Data.Text.Text,
    ticketShortId :: Data.Text.Text,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingPeopleCategoryDetails = TicketBookingPeopleCategoryDetails
  { name :: Data.Text.Text,
    numberOfUnits :: Kernel.Prelude.Int,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingPeopleCategoryReq = TicketBookingPeopleCategoryReq
  { numberOfUnits :: Kernel.Prelude.Int,
    peopleCategoryId :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingReq = TicketBookingReq
  { services :: [Domain.Action.UI.TicketService.TicketBookingServicesReq],
    visitDate :: Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingServiceDetails = TicketBookingServiceDetails
  { amount :: Kernel.Types.Common.HighPrecMoney,
    categories :: [Domain.Action.UI.TicketService.TicketBookingCategoryDetails],
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    ticketServiceShortId :: Data.Text.Text,
    slot :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    status :: Domain.Types.TicketBookingService.ServiceStatus,
    ticketServiceName :: Data.Text.Text,
    verificationCount :: Kernel.Prelude.Int,
    -- deprecated fields
    prices :: [TicketBookingServicePriceBreakup]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingServicesReq = TicketBookingServicesReq
  { businessHourId :: Maybe (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour), -- handle backward compatibility
    categories :: Maybe [Domain.Action.UI.TicketService.TicketBookingCategoryReq],
    serviceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    -- deprecated fields
    attendeeType :: Maybe Data.Text.Text,
    numberOfUnits :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingUpdateSeatsReq = TicketBookingUpdateSeatsReq
  { businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    categoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    date :: Data.Time.Calendar.Day,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    updatedBookedSeats :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- deprecated types
data TicketServicePrice = TicketServicePrice
  { ticketServiceId :: Data.Text.Text,
    attendeeType :: Data.Text.Text,
    pricePerUnit :: HighPrecMoney
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- deprecated types
data TicketBookingServicePriceBreakup = TicketBookingServicePriceBreakup
  { ticketBookingServiceId :: Kernel.Types.Id.Id DTB.TicketBookingService,
    attendeeType :: Data.Text.Text,
    numberOfUnits :: Int,
    pricePerUnit :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TicketServiceResp = TicketServiceResp
  { allowFutureBooking :: Kernel.Prelude.Bool,
    businessHours :: [Domain.Action.UI.TicketService.BusinessHourResp],
    expiry :: Domain.Types.TicketService.ExpiryType,
    id :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    maxVerification :: Kernel.Prelude.Int,
    name :: Data.Text.Text,
    placesId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    -- deprecated fields
    service :: Data.Text.Text,
    openTimings :: Maybe Kernel.Prelude.TimeOfDay,
    closeTimings :: Maybe Kernel.Prelude.TimeOfDay,
    validityTimings :: Maybe Kernel.Prelude.TimeOfDay,
    prices :: [TicketServicePrice]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketServiceVerificationResp = TicketServiceVerificationResp
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    categories :: [Domain.Action.UI.TicketService.TicketBookingCategoryDetails],
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    message :: Data.Text.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    status :: Domain.Action.UI.TicketService.TicketVerificationStatus,
    ticketServiceName :: Kernel.Prelude.Maybe Data.Text.Text,
    ticketServiceShortId :: Kernel.Prelude.Maybe Data.Text.Text,
    units :: Kernel.Prelude.Maybe [TicketBookingServicePriceBreakup],
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verificationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketVerificationStatus = BookingSuccess | BookingExpired | BookingFuture | BookingAlreadyVerified | DifferentService | PaymentPending | InvalidBooking
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ConvertedTime = ConvertedTime
  { slot :: Maybe Kernel.Prelude.TimeOfDay,
    startTime :: Maybe Kernel.Prelude.TimeOfDay,
    endTime :: Maybe Kernel.Prelude.TimeOfDay
  }

convertBusinessHT :: Domain.Types.BusinessHour.BusinessHourType -> ConvertedTime
convertBusinessHT (Domain.Types.BusinessHour.Slot time) = ConvertedTime {slot = Just time, startTime = Nothing, endTime = Nothing}
convertBusinessHT (Domain.Types.BusinessHour.Duration startTime endTime) = ConvertedTime {slot = Nothing, startTime = Just startTime, endTime = Just endTime}

getTicketPlaces :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces (_, merchantId) = do
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId
  QTP.getTicketPlaces merchantOpCity.id

getTicketPlacesServices :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.Flow [Domain.Action.UI.TicketService.TicketServiceResp]
getTicketPlacesServices placeId = do
  ticketServices <- QTS.getTicketServicesByPlaceId placeId.getId
  now <- getCurrentTime
  let currentDay = dayOfWeek $ utctDay now
      currentDate = utctDay now
  specialOccasions <- mapM (findSpecialOccasion currentDay currentDate) ticketServices
  let ticketServicesWSpOcc = zip ticketServices specialOccasions
  mkTicketServiceListRes ticketServicesWSpOcc currentDate placeId
  where
    mkTicketServiceListRes ticketServicesWSpOcc currentDate_ pId =
      mapM
        ( \(service, mbSpecialOcc) -> do
            let bhIds = maybe (service.businessHours) (.businessHours) mbSpecialOcc
            businessHours <- mapM (mkBusinessHoursRes service currentDate_ mbSpecialOcc) bhIds
            pure $
              TicketServiceResp
                { id = service.id,
                  placesId = pId,
                  name = service.service,
                  maxVerification = service.maxVerification,
                  allowFutureBooking = service.allowFutureBooking,
                  expiry = service.expiry,
                  businessHours,
                  -- deprecated fields
                  service = service.service,
                  openTimings = Kernel.Prelude.listToMaybe businessHours >>= (.startTime),
                  closeTimings = Kernel.Prelude.listToMaybe businessHours >>= (.endTime),
                  validityTimings =
                    case service.expiry of
                      Domain.Types.TicketService.InstantExpiry _ -> Nothing
                      Domain.Types.TicketService.VisitDate timeOfDay -> Just timeOfDay,
                  prices = mkPrices service.id businessHours
                }
        )
        ticketServicesWSpOcc

    mkPrices :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> [BusinessHourResp] -> [TicketServicePrice]
    mkPrices serviceId businessHours = do
      let mbPeopleCategories = Kernel.Prelude.listToMaybe businessHours >>= Just . (.categories) >>= Kernel.Prelude.listToMaybe >>= Just . (.peopleCategories)
      map (mkPrice serviceId) (fromMaybe [] mbPeopleCategories)

    mkPrice :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> PeopleCategoriesResp -> TicketServicePrice
    mkPrice serviceId PeopleCategoriesResp {..} =
      TicketServicePrice
        { ticketServiceId = Kernel.Types.Id.getId serviceId,
          attendeeType = name,
          pricePerUnit = pricePerUnit
        }

    mkBusinessHoursRes service currDate mbSpecialOcc bhId = do
      businessHour <- QBH.findById bhId >>= fromMaybeM (BusinessHourNotFound bhId.getId)
      let convertedBusinessHT = convertBusinessHT businessHour.btype
          mbOperationalDay = (.dayOfWeek) =<< mbSpecialOcc
      categories <- mapM (mkServiceCategories currDate) businessHour.categoryId
      pure $
        BusinessHourResp
          { id = bhId,
            slot = convertedBusinessHT.slot,
            startTime = convertedBusinessHT.startTime,
            endTime = convertedBusinessHT.endTime,
            specialDayDescription = (.description) =<< mbSpecialOcc,
            specialDayType = (.specialDayType) <$> mbSpecialOcc,
            operationalDays = maybe service.operationalDays (: []) mbOperationalDay,
            categories
          }

    mkServiceCategories curDate serviceCatId = do
      serviceCategory <- QSC.findById serviceCatId >>= fromMaybeM (ServiceCategoryNotFound serviceCatId.getId)
      mBeatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId curDate
      peopleCategories <- mapM mkPeopleCategoriesRes serviceCategory.peopleCategory
      pure $
        CategoriesResp
          { name = serviceCategory.name,
            id = serviceCategory.id,
            availableSeats = serviceCategory.availableSeats,
            bookedSeats = maybe 0 (.booked) mBeatManagement,
            allowedSeats = calcAllowedSeats serviceCategory mBeatManagement,
            peopleCategories
          }

    mkPeopleCategoriesRes pCatId = do
      peopleCategory <- QPC.findById pCatId >>= fromMaybeM (PeopleCategoryNotFound pCatId.getId)
      pure $
        PeopleCategoriesResp
          { name = peopleCategory.name,
            id = peopleCategory.id,
            pricePerUnit = peopleCategory.pricePerUnit,
            description = peopleCategory.description
          }

    findSpecialOccasion curDay curDate service = do
      maybeOccasionByDate <- QSO.findSpecialOccasionByEntityIdAndDate service.id.getId (Just curDate)
      case maybeOccasionByDate of
        Just occ -> pure (Just occ)
        Nothing -> QSO.findSpecialOccasionByEntityIdAndDayOfWeek service.id.getId (Just $ show curDay)

    calcAllowedSeats serC mSeatM =
      case serC.availableSeats of
        Nothing -> Nothing
        Just availableSeats -> Just $ availableSeats - (maybe 0 (.booked) mSeatM) - (maybe 0 (.blocked) mSeatM)

postTicketPlacesBook :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Action.UI.TicketService.TicketBookingReq -> Environment.Flow Kernel.External.Payment.Interface.Types.CreateOrderResp
postTicketPlacesBook (personId, merchantId) placeId req' = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId

  -- to handle backward compatability fill in new data
  req <-
    case (Kernel.Prelude.head req'.services).businessHourId of
      Nothing -> backfillTicketServiceReq req'
      Just _ -> return req'

  ticketBookingId <- generateGUID
  ticketBookingServices <- mapM (createTicketBookingService req merchantOpCity.id ticketBookingId) req.services

  let amount = sum (map (.amount) ticketBookingServices)
  ticketBooking <- createTicketBooking req merchantOpCity.id ticketBookingId amount

  QTBS.createMany ticketBookingServices
  QTB.create ticketBooking

  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = ticketBooking.id.getId,
            orderShortId = ticketBooking.shortId.getShortId,
            amount = amount,
            customerId = personId.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = personPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing
          }
  let commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder merchantId
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return createOrderRes
    Nothing -> do
      throwError $ InternalError "Failed to create order"
  where
    createTicketBooking req merchantOperatingCityId ticketBookingId amount = do
      shortId <- generateShortId
      now <- getCurrentTime
      return $
        Domain.Types.TicketBooking.TicketBooking
          { id = ticketBookingId,
            shortId,
            merchantOperatingCityId,
            ticketPlaceId = placeId,
            personId = Kernel.Types.Id.cast personId,
            amount,
            visitDate = req.visitDate,
            status = DTTB.Pending,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingService req merchantOperatingCityId ticketBookingId ticketServicesReq = do
      let ticketServiceId = ticketServicesReq.serviceId
      bHourId <- ticketServicesReq.businessHourId & fromMaybeM (BusinessHourNotFound "No business hours found")
      categories <- ticketServicesReq.categories & fromMaybeM (ServiceCategoryNotFound "No categories found")
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      ticketService <- QTS.findById ticketServicesReq.serviceId >>= fromMaybeM (TicketServiceNotFound ticketServicesReq.serviceId.getId)
      businessHour <- QBH.findById bHourId >>= fromMaybeM (BusinessHourNotFound bHourId.getId)
      tBookingSCats <- mapM (createTicketBookingServiceCategory id) categories
      QTBSC.createMany tBookingSCats
      let amount = sum (map (.amount) tBookingSCats)
      let expiry = calcExpiry ticketService.expiry req.visitDate now
      return $
        DTB.TicketBookingService
          { id,
            shortId,
            ticketBookingId,
            ticketServiceId,
            amount,
            btype = businessHour.btype,
            status = DTB.Pending,
            verificationCount = 0,
            expiryDate = Just expiry,
            merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingServiceCategory ticketBookingServiceId ticketServiceCReq = do
      id <- generateGUID
      let serviceCatId = ticketServiceCReq.categoryId
      tBookingSC <- QSC.findById serviceCatId >>= fromMaybeM (ServiceCategoryNotFound serviceCatId.getId)
      tBookingPCats <- mapM (createTicketBookingPeopleCategory id) ticketServiceCReq.peopleCategories
      let (amount, bookedSeats) = calculateAmountAndSeats tBookingPCats
      QTBPC.createMany tBookingPCats

      mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId req'.visitDate
      if isJust mbSeatM
        then QTSM.updateBookedSeats bookedSeats serviceCatId req'.visitDate
        else do
          seatId <- generateGUID
          let seatM =
                Domain.Types.SeatManagement.SeatManagement
                  { id = seatId,
                    ticketServiceCategoryId = serviceCatId,
                    date = req'.visitDate,
                    blocked = 0,
                    booked = bookedSeats
                  }
          QTSM.create seatM

      return $
        DTB.TicketBookingServiceCategory
          { id,
            name = tBookingSC.name,
            ticketBookingServiceId,
            bookedSeats,
            amount
          }

    createTicketBookingPeopleCategory ticketBookingServiceCategoryId ticketServicePCReq = do
      id <- generateGUID
      let tPCatId = ticketServicePCReq.peopleCategoryId
      tServicePCat <- QPC.findById tPCatId >>= fromMaybeM (PeopleCategoryNotFound tPCatId.getId)
      let numberOfUnits = ticketServicePCReq.numberOfUnits
          pricePerUnit = tServicePCat.pricePerUnit
      return $
        DTB.TicketBookingPeopleCategory
          { id,
            name = tServicePCat.name,
            ticketBookingServiceCategoryId,
            numberOfUnits,
            pricePerUnit
          }

    calculateAmountAndSeats :: [DTB.TicketBookingPeopleCategory] -> (HighPrecMoney, Int)
    calculateAmountAndSeats categories =
      foldl
        ( \(totalAmount, totalSeats) category ->
            let categoryAmount = category.pricePerUnit * fromIntegral (category.numberOfUnits)
             in (totalAmount + categoryAmount, totalSeats + category.numberOfUnits)
        )
        (HighPrecMoney 0, 0)
        categories

    calcExpiry :: Domain.Types.TicketService.ExpiryType -> Data.Time.Calendar.Day -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime
    calcExpiry expiry visitDate currentTime = case expiry of
      Domain.Types.TicketService.InstantExpiry minutes -> addUTCTime (fromIntegral (minutes * 60)) currentTime
      Domain.Types.TicketService.VisitDate timeOfDay -> UTCTime visitDate (timeOfDayToTime timeOfDay)

    -- temporary code for backfilling
    backfillTicketServiceReq :: Domain.Action.UI.TicketService.TicketBookingReq -> Environment.Flow Domain.Action.UI.TicketService.TicketBookingReq
    backfillTicketServiceReq (TicketBookingReq services' visitDate) = do
      let peopleCategoriesArray = map DLN.toList $ groupBy ((==) `on` serviceId) services'
      services <-
        mapM
          ( \peopleCategories -> do
              let ticketServiceId = (.serviceId) (Kernel.Prelude.head peopleCategories)
              ticketService <- QTS.findById ticketServiceId >>= fromMaybeM (TicketServiceNotFound ticketServiceId.getId)
              backfillTicketBookingServiceReq peopleCategories ticketService
          )
          peopleCategoriesArray
      return $
        TicketBookingReq
          { services,
            visitDate = visitDate
          }

    backfillTicketBookingServiceReq :: [TicketBookingServicesReq] -> Domain.Types.TicketService.TicketService -> Environment.Flow TicketBookingServicesReq
    backfillTicketBookingServiceReq peopleCategories ticketService = do
      businessHourId <- Kernel.Prelude.listToMaybe ticketService.businessHours & fromMaybeM (BusinessHourNotFound "No business hours found")
      businessHour <- QBH.findById businessHourId >>= fromMaybeM (BusinessHourNotFound businessHourId.getId)
      categoryId <- Kernel.Prelude.listToMaybe businessHour.categoryId & fromMaybeM (ServiceCategoryNotFound "No categories found")
      category <- createTicketBookingCategoryReq categoryId peopleCategories
      return $
        TicketBookingServicesReq
          { businessHourId = Just businessHourId,
            categories = Just [category],
            serviceId = ticketService.id,
            attendeeType = Nothing,
            numberOfUnits = Nothing
          }

    createTicketBookingCategoryReq :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> [TicketBookingServicesReq] -> Environment.Flow TicketBookingCategoryReq
    createTicketBookingCategoryReq cid peopleCategories' = do
      categoryConfig <- QSC.findById cid >>= fromMaybeM (ServiceCategoryNotFound cid.getId)
      peopleCategoriesConfigs <- mapM QPC.findById categoryConfig.peopleCategory
      peopleCategories <- mapM (createPeopleCategory (catMaybes peopleCategoriesConfigs)) peopleCategories'
      return $
        TicketBookingCategoryReq
          { categoryId = cid,
            peopleCategories
          }

    createPeopleCategory :: [Domain.Types.ServicePeopleCategory.ServicePeopleCategory] -> TicketBookingServicesReq -> Environment.Flow TicketBookingPeopleCategoryReq
    createPeopleCategory peopleCategoriesConfigs TicketBookingServicesReq {..} = do
      peopleCategory <- find (\pc -> Just pc.name == attendeeType) peopleCategoriesConfigs & fromMaybeM (PeopleCategoryNotFound "No people category found")
      return $
        TicketBookingPeopleCategoryReq
          { numberOfUnits = fromMaybe 0 numberOfUnits,
            peopleCategoryId = peopleCategory.id
          }

getTicketBookings :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.Flow [Domain.Action.UI.TicketService.TicketBookingAPIEntity]
getTicketBookings (personId_, merchantId) mbLimit mbOffset status_ = do
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId
  ticketBookings <- QTB.getAllBookingsByPersonId mbLimit mbOffset personId_ merchantOpCity.id status_
  convertToApiEntity `mapM` ticketBookings
  where
    convertToApiEntity :: DTTB.TicketBooking -> Environment.Flow TicketBookingAPIEntity
    convertToApiEntity DTTB.TicketBooking {..} = do
      ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (TicketPlaceNotFound ticketPlaceId.getId)
      return $
        TicketBookingAPIEntity
          { ticketShortId = Kernel.Types.Id.getShortId shortId,
            ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
            personId = personId.getId,
            ticketPlaceName = ticketPlace.name,
            ..
          }

getTicketBookingsDetails :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Action.UI.TicketService.TicketBookingDetails
getTicketBookingsDetails _ shortId_ = do
  ticketBooking <- QTB.findByShortId shortId_ >>= fromMaybeM (TicketBookingNotFound shortId_.getShortId)
  ticketBookingServices <- QTBS.findAllByBookingId ticketBooking.id
  services <- mapM mkTicketBookingServiceDetails ticketBookingServices
  mkTicketBookingDetails ticketBooking services
  where
    mkTicketBookingDetails DTTB.TicketBooking {..} services = do
      ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (TicketPlaceNotFound ticketPlaceId.getId)
      return $
        TicketBookingDetails
          { ticketShortId = shortId.getShortId,
            ticketPlaceId = ticketPlaceId.getId,
            personId = personId.getId,
            ticketPlaceName = ticketPlace.name,
            ..
          }

    mkTicketBookingServiceDetails DTB.TicketBookingService {..} = do
      ticketService <- QTS.findById ticketServiceId >>= fromMaybeM (TicketServiceNotFound ticketServiceId.getId)
      serviceCategories <- QTBSC.findAllByTicketBookingServiceId id
      categoryDetails <- mapM mkTicketBookingCategoryDetails serviceCategories
      let convertedBH = convertBusinessHT btype
      return $
        TicketBookingServiceDetails
          { categories = categoryDetails,
            ticketServiceShortId = shortId.getShortId,
            slot = convertedBH.slot,
            ticketServiceName = ticketService.service,
            prices = fromMaybe [] $ mkUnits categoryDetails (Just id),
            ..
          }

    mkTicketBookingCategoryDetails DTB.TicketBookingServiceCategory {..} = do
      peopleCategories <- QTBPC.findAllByServiceCategoryId id
      peopleCategoryDetails <- mapM (mkTicketBookingPeopleCategoryDetails) peopleCategories
      return $
        TicketBookingCategoryDetails
          { peopleCategories = peopleCategoryDetails,
            ..
          }

    mkTicketBookingPeopleCategoryDetails DTB.TicketBookingPeopleCategory {..} = do
      return $
        TicketBookingPeopleCategoryDetails
          { ..
          }

postTicketBookingsVerify :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow Domain.Action.UI.TicketService.TicketServiceVerificationResp
postTicketBookingsVerify = processBookingService
  where
    processBookingService :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow Domain.Action.UI.TicketService.TicketServiceVerificationResp
    processBookingService ticketServiceId bookingServiceShortId = do
      mBookingService <- QTBS.findByShortId bookingServiceShortId
      case mBookingService of
        Just bookingService -> do
          (mbTicketService, mbBooking) <- liftM2 (,) (QTS.findById bookingService.ticketServiceId) (QTB.findById bookingService.ticketBookingId)
          case (mbTicketService, mbBooking) of
            (Just ticketService, Just booking) -> processValidBooking bookingService ticketService booking ticketServiceId
            _ -> createVerificationResp InvalidBooking Nothing Nothing Nothing
        Nothing -> createVerificationResp InvalidBooking Nothing Nothing Nothing

    processValidBooking :: DTB.TicketBookingService -> Domain.Types.TicketService.TicketService -> DTTB.TicketBooking -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.Flow TicketServiceVerificationResp
    processValidBooking bookingService ticketService booking ticketServiceId
      | bookingService.ticketServiceId /= ticketServiceId = createVerificationResp DifferentService Nothing (Just ticketService) Nothing
      | otherwise = case bookingService.status of
        DTB.Pending -> createVerificationResp PaymentPending (Just bookingService) (Just ticketService) (Just booking)
        DTB.Failed -> createVerificationResp InvalidBooking (Just bookingService) (Just ticketService) (Just booking)
        DTB.Verified -> handleConfirmedBooking bookingService ticketService booking
        DTB.Confirmed -> handleConfirmedBooking bookingService ticketService booking

    handleConfirmedBooking :: DTB.TicketBookingService -> Domain.Types.TicketService.TicketService -> DTTB.TicketBooking -> Environment.Flow TicketServiceVerificationResp
    handleConfirmedBooking bookingService ticketServiceConfig booking = do
      now <- getCurrentTime
      case bookingService.expiryDate of
        Just expiry ->
          if expiry < now
            then createVerificationResp BookingExpired (Just bookingService) (Just ticketServiceConfig) (Just booking)
            else handleConfirmedNonExpiredBooking bookingService ticketServiceConfig booking
        Nothing -> handleConfirmedNonExpiredBooking bookingService ticketServiceConfig booking

    handleConfirmedNonExpiredBooking :: DTB.TicketBookingService -> Domain.Types.TicketService.TicketService -> DTTB.TicketBooking -> Environment.Flow TicketServiceVerificationResp
    handleConfirmedNonExpiredBooking bookingService ticketServiceConfig booking = do
      now <- getCurrentTime
      if booking.visitDate > utctDay now
        then do createVerificationResp BookingFuture (Just bookingService) (Just ticketServiceConfig) (Just booking)
        else do handleVerifiedBooking bookingService ticketServiceConfig booking

    handleVerifiedBooking :: DTB.TicketBookingService -> Domain.Types.TicketService.TicketService -> DTTB.TicketBooking -> Environment.Flow TicketServiceVerificationResp
    handleVerifiedBooking bookingService ticketServiceConfig booking
      | bookingService.verificationCount >= ticketServiceConfig.maxVerification =
        createVerificationResp BookingAlreadyVerified (Just bookingService) (Just ticketServiceConfig) (Just booking)
      | otherwise = do
        now <- getCurrentTime
        QTBS.updateVerificationById DTB.Verified (bookingService.verificationCount + 1) now bookingService.id
        createVerificationResp BookingSuccess (Just bookingService) (Just ticketServiceConfig) (Just booking)

    createVerificationResp :: TicketVerificationStatus -> Maybe DTB.TicketBookingService -> Maybe Domain.Types.TicketService.TicketService -> Maybe DTTB.TicketBooking -> Environment.Flow TicketServiceVerificationResp
    createVerificationResp status mbBookingService mbTicketService mbBooking = do
      let bType = maybe Nothing (\booking -> Just booking.btype) mbBookingService
          mbConvertedT = convertBusinessHT <$> bType
          mbTicketServiceId = (.id) <$> mbBookingService
      serviceCategories <- traverse (QTBSC.findAllByTicketBookingServiceId) mbTicketServiceId
      serviceCatDetails <- mkTicketBookingCategoryDetails serviceCategories
      pure $
        TicketServiceVerificationResp
          { ticketServiceName = mbTicketService <&> (.service),
            visitDate = mbBooking <&> (.visitDate),
            validTill = mbBookingService >>= (.expiryDate) >>= (Just . addUTCTime (secondsToNominalDiffTime 19800)), -- 19800 for +5:30 timezone
            ticketServiceShortId = mbBookingService <&> (.shortId) <&> (.getShortId),
            message = verificationMsg status,
            status,
            amount = mbBookingService <&> (.amount),
            verificationCount = mbBookingService <&> (.verificationCount),
            units = mkUnits serviceCatDetails mbTicketServiceId, --deprecated
            startTime = findStartTime mbConvertedT,
            endTime = (.endTime) =<< mbConvertedT,
            categories = serviceCatDetails
          }

    mkTicketBookingCategoryDetails :: Maybe [DTB.TicketBookingServiceCategory] -> Environment.Flow [TicketBookingCategoryDetails]
    mkTicketBookingCategoryDetails serviceCategories = do
      case serviceCategories of
        Nothing -> pure []
        Just list ->
          mapM
            ( \category -> do
                peopleCategories <- QTBPC.findAllByServiceCategoryId category.id
                let peopleCategoryDetails = map mkTicketBookingPeopleCategoryDetails peopleCategories
                return
                  TicketBookingCategoryDetails
                    { peopleCategories = peopleCategoryDetails,
                      amount = category.amount,
                      bookedSeats = category.bookedSeats,
                      name = category.name
                    }
            )
            list

    mkTicketBookingPeopleCategoryDetails :: DTB.TicketBookingPeopleCategory -> TicketBookingPeopleCategoryDetails
    mkTicketBookingPeopleCategoryDetails DTB.TicketBookingPeopleCategory {..} =
      TicketBookingPeopleCategoryDetails
        { ..
        }

    findStartTime :: Kernel.Prelude.Maybe ConvertedTime -> Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay
    findStartTime maybeConvertedTime =
      case maybeConvertedTime of
        Just convertedTime ->
          case (convertedTime.slot, convertedTime.startTime) of
            (Nothing, Just st) -> Just st
            (st, Nothing) -> st
            _ -> Nothing
        Nothing -> Nothing

    verificationMsg :: TicketVerificationStatus -> Data.Text.Text
    verificationMsg BookingSuccess = "Validated successfully!"
    verificationMsg BookingExpired = "Booking Expired!"
    verificationMsg BookingFuture = "Booking for Later Date!"
    verificationMsg BookingAlreadyVerified = "Already Validated!"
    verificationMsg DifferentService = "Different Service!"
    verificationMsg PaymentPending = "Payment Pending!"
    verificationMsg InvalidBooking = "Not a valid QR"

-- deprecated function
mkUnits :: [TicketBookingCategoryDetails] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id DTB.TicketBookingService) -> Maybe [TicketBookingServicePriceBreakup]
mkUnits serviceCatDetails mbTicketBookingServiceId = do
  case serviceCatDetails of
    [] -> Nothing
    (x : _) ->
      Just $
        map
          ( \peopleCat ->
              TicketBookingServicePriceBreakup
                { ticketBookingServiceId = Kernel.Types.Id.cast $ fromMaybe (Kernel.Types.Id.Id "") mbTicketBookingServiceId,
                  attendeeType = peopleCat.name,
                  numberOfUnits = peopleCat.numberOfUnits,
                  pricePerUnit = peopleCat.pricePerUnit
                }
          )
          x.peopleCategories

getTicketBookingsStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Types.TicketBooking.BookingStatus
getTicketBookingsStatus (personId, merchantId) _shortId@(Kernel.Types.Id.ShortId shortId) = do
  let commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  order <- QOrder.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (PaymentOrderNotFound shortId)
  ticketBooking' <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId)
  if order.status == Payment.CHARGED -- Consider CHARGED status as terminal status
    then return ticketBooking'.status
    else do
      now <- getCurrentTime
      paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
      case paymentStatus of
        DPayment.PaymentStatus {..} -> do
          when (status == Payment.CHARGED) $ do
            QTB.updateStatusByShortId DTTB.Booked now _shortId
            QTBS.updateAllStatusByBookingId DTB.Confirmed now ticketBooking'.id
          when (status `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]) $ do
            QTB.updateStatusByShortId DTTB.Failed now _shortId
            QTBS.updateAllStatusByBookingId DTB.Failed now ticketBooking'.id
        _ -> return ()
      ticketBooking <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId) -- fetch again for updated status
      return ticketBooking.status

postTicketBookingsUpdateSeats :: Domain.Action.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketBookingsUpdateSeats TicketBookingUpdateSeatsReq {..} = do
  void $ QTSM.updateBookedSeats updatedBookedSeats categoryId date
  pure Kernel.Types.APISuccess.Success
