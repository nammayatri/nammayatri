{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.TicketService where

import API.Types.UI.TicketService
import qualified Data.List.NonEmpty as DLN
import Data.OpenApi (ToSchema)
import qualified Data.Text as Data.Text
import Data.Time (UTCTime (UTCTime), dayOfWeek, midnight, timeOfDayToTime, utctDay)
import qualified Data.Time.Calendar as Data.Time.Calendar
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
import Storage.Beam.Payment ()
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

data ConvertedTime = ConvertedTime
  { slot :: Maybe Kernel.Prelude.TimeOfDay,
    startTime :: Maybe Kernel.Prelude.TimeOfDay,
    endTime :: Maybe Kernel.Prelude.TimeOfDay
  }

convertBusinessHT :: Domain.Types.BusinessHour.BusinessHourType -> ConvertedTime
convertBusinessHT (Domain.Types.BusinessHour.Slot time) = ConvertedTime {slot = Just time, startTime = Nothing, endTime = Nothing}
convertBusinessHT (Domain.Types.BusinessHour.Duration startTime endTime) = ConvertedTime {slot = Nothing, startTime = Just startTime, endTime = Just endTime}

getTicketPlaces :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces (_, merchantId) = do
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId
  QTP.getTicketPlaces (Just merchantOpCity.id)

getTicketPlacesServices :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
getTicketPlacesServices _ placeId mbDate = do
  ticketServices <- QTS.getTicketServicesByPlaceId placeId.getId
  now <- getCurrentTime
  let bookingDate = fromMaybe (utctDay now) mbDate
  mkTicketServiceListRes ticketServices bookingDate placeId
  where
    mkTicketServiceListRes ticketServices bookingDate_ pId =
      mapM
        ( \service -> do
            specialOccasions <- findSpecialOccasion service
            let getBusinessHourForSpecialLocation sl businessHours = if null businessHours then map (\bh -> (sl, bh)) service.businessHours else map (\bh -> (sl, bh)) businessHours
            let specialOccBHourIds = concat (map (\sl -> getBusinessHourForSpecialLocation sl sl.businessHours) specialOccasions)
            specialOccBHours <- mapM (\(specialOcc, bhId) -> mkBusinessHoursRes service bookingDate_ (Just specialOcc) bhId) specialOccBHourIds
            normalBusinessHours <- mapM (mkBusinessHoursRes service bookingDate_ Nothing) service.businessHours
            let businessHours = normalBusinessHours ++ specialOccBHours
            pure $
              TicketServiceResp
                { id = service.id,
                  placesId = pId,
                  name = service.service,
                  maxVerification = service.maxVerification,
                  allowFutureBooking = service.allowFutureBooking,
                  expiry = service.expiry,
                  businessHours,
                  shortDesc = service.shortDesc
                }
        )
        ticketServices

    mkBusinessHoursRes :: Domain.Types.TicketService.TicketService -> Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Domain.Types.SpecialOccasion.SpecialOccasion -> Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Environment.Flow BusinessHourResp
    mkBusinessHoursRes service bDate mbSpecialOcc bhId = do
      businessHour <- QBH.findById bhId >>= fromMaybeM (BusinessHourNotFound bhId.getId)
      let convertedBusinessHT = convertBusinessHT businessHour.btype
          mbOperationalDay = (.dayOfWeek) =<< mbSpecialOcc
      categories <- mapM (mkServiceCategories bDate) businessHour.categoryId
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

    mkServiceCategories bDate_ serviceCatId = do
      serviceCategory <- QSC.findById serviceCatId >>= fromMaybeM (ServiceCategoryNotFound serviceCatId.getId)
      mBeatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId bDate_
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

    findSpecialOccasion service = QSO.findAllSpecialOccasionByEntityId service.id.getId Nothing

    calcAllowedSeats serC mSeatM =
      case serC.availableSeats of
        Nothing -> Nothing
        Just availableSeats -> Just $ availableSeats - (maybe 0 (.booked) mSeatM) - (maybe 0 (.blocked) mSeatM)

postTicketPlacesBook :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> API.Types.UI.TicketService.TicketBookingReq -> Environment.Flow Kernel.External.Payment.Interface.Types.CreateOrderResp
postTicketPlacesBook (mbPersonId, merchantId) placeId req = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId_ >>= fromMaybeM (PersonNotFound personId_.getId)
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId

  ticketBookingId <- generateGUID
  ticketBookingServices <- mapM (createTicketBookingService merchantOpCity.id ticketBookingId) req.services

  let amount = sum (map (.amount) ticketBookingServices)
  ticketBooking <- createTicketBooking personId_ merchantOpCity.id ticketBookingId amount

  QTBS.createMany ticketBookingServices
  QTB.create ticketBooking

  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = ticketBooking.id.getId,
            orderShortId = ticketBooking.shortId.getShortId,
            amount = amount,
            customerId = personId_.getId,
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
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId_
      createOrderCall = Payment.createOrder merchantId
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return createOrderRes
    Nothing -> do
      throwError $ InternalError "Failed to create order"
  where
    createTicketBooking personId_ merchantOperatingCityId ticketBookingId amount = do
      shortId <- generateShortId
      now <- getCurrentTime
      return $
        Domain.Types.TicketBooking.TicketBooking
          { id = ticketBookingId,
            shortId,
            ticketPlaceId = placeId,
            personId = Kernel.Types.Id.cast personId_,
            amount,
            visitDate = req.visitDate,
            status = DTTB.Pending,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingService merchantOperatingCityId ticketBookingId ticketServicesReq = do
      let ticketServiceId = ticketServicesReq.serviceId
      let bHourId = ticketServicesReq.businessHourId
      let categories = ticketServicesReq.categories
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      ticketService <- QTS.findById ticketServicesReq.serviceId >>= fromMaybeM (TicketServiceNotFound ticketServicesReq.serviceId.getId)
      businessHour <- QBH.findById bHourId >>= fromMaybeM (BusinessHourNotFound bHourId.getId)
      tBookingSCats <- mapM (createTicketBookingServiceCategory merchantOperatingCityId id) categories
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
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingServiceCategory merchantOperatingCityId ticketBookingServiceId ticketServiceCReq = do
      id <- generateGUID
      now <- getCurrentTime
      let serviceCatId = ticketServiceCReq.categoryId
      tBookingSC <- QSC.findById serviceCatId >>= fromMaybeM (ServiceCategoryNotFound serviceCatId.getId)
      tBookingPCats <- mapM (createTicketBookingPeopleCategory now merchantOperatingCityId id) ticketServiceCReq.peopleCategories
      let (amount, bookedSeats) = calculateAmountAndSeats tBookingPCats
      QTBPC.createMany tBookingPCats

      mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId req.visitDate
      if isJust mbSeatM
        then QTSM.updateBlockedSeats bookedSeats serviceCatId req.visitDate
        else do
          seatId <- generateGUID
          let seatM =
                Domain.Types.SeatManagement.SeatManagement
                  { id = seatId,
                    ticketServiceCategoryId = serviceCatId,
                    date = req.visitDate,
                    blocked = bookedSeats,
                    booked = 0,
                    merchantId = Just merchantId,
                    merchantOperatingCityId = Just merchantOperatingCityId,
                    createdAt = now,
                    updatedAt = now
                  }
          QTSM.create seatM

      return $
        DTB.TicketBookingServiceCategory
          { id,
            name = tBookingSC.name,
            ticketBookingServiceId,
            bookedSeats,
            amount,
            serviceCategoryId = Just $ Kernel.Types.Id.getId tBookingSC.id,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingPeopleCategory now merchantOperatingCityId ticketBookingServiceCategoryId ticketServicePCReq = do
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
            pricePerUnit,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
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

getTicketBookings :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.Flow [API.Types.UI.TicketService.TicketBookingAPIEntity]
getTicketBookings (mbPersonId, merchantId_) mbLimit mbOffset status_ = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId_
  ticketBookings <- QTB.getAllBookingsByPersonId mbLimit mbOffset personId_ (Just merchantOpCity.id) status_
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

getTicketBookingsDetails :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow API.Types.UI.TicketService.TicketBookingDetails
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

postTicketBookingsVerify :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp
postTicketBookingsVerify _ = processBookingService
  where
    processBookingService :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp
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
        QTBS.updateVerificationById DTB.Verified (bookingService.verificationCount + 1) bookingService.id
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

getTicketBookingsStatus :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Types.TicketBooking.BookingStatus
getTicketBookingsStatus (mbPersonId, merchantId) _shortId@(Kernel.Types.Id.ShortId shortId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  let commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  order <- QOrder.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (PaymentOrderNotFound shortId)
  ticketBooking' <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId)
  ticketBookingServices <- QTBS.findAllByBookingId ticketBooking'.id
  tBookingServiceCats <- mapM (\tBookingS -> QTBSC.findAllByTicketBookingServiceId tBookingS.id) ticketBookingServices
  let ticketBookingServiceCategories = concat tBookingServiceCats
  if order.status == Payment.CHARGED -- Consider CHARGED status as terminal status
    then return ticketBooking'.status
    else do
      paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
      case paymentStatus of
        DPayment.PaymentStatus {..} -> do
          when (status == Payment.CHARGED) $ do
            QTB.updateStatusByShortId DTTB.Booked _shortId
            QTBS.updateAllStatusByBookingId DTB.Confirmed ticketBooking'.id
            mapM_
              ( \tbsc ->
                  whenJust tbsc.serviceCategoryId $ \serviceId ->
                    updateBookedAndBlockedSeats (Kernel.Types.Id.Id serviceId) tbsc ticketBooking'.visitDate
              )
              ticketBookingServiceCategories
          when (status `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]) $ do
            QTB.updateStatusByShortId DTTB.Failed _shortId
            QTBS.updateAllStatusByBookingId DTB.Failed ticketBooking'.id
            mapM_
              ( \tbsc ->
                  whenJust tbsc.serviceCategoryId $ \serviceId ->
                    updateBlockedSeats (Kernel.Types.Id.Id serviceId) tbsc ticketBooking'.visitDate
              )
              ticketBookingServiceCategories
        _ -> return ()
      ticketBooking <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId) -- fetch again for updated status
      return ticketBooking.status
  where
    updateBookedAndBlockedSeats :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> DTB.TicketBookingServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow ()
    updateBookedAndBlockedSeats serviceCatId tbsc visitDate = do
      seatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId visitDate >>= fromMaybeM (TicketSeatManagementNotFound serviceCatId.getId (show visitDate))
      QTSM.updateBlockedSeats (seatManagement.blocked - tbsc.bookedSeats) serviceCatId visitDate
      QTSM.updateBookedSeats (seatManagement.booked + tbsc.bookedSeats) serviceCatId visitDate

    updateBlockedSeats :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> DTB.TicketBookingServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow ()
    updateBlockedSeats serviceCatId tbsc visitDate = do
      seatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId visitDate >>= fromMaybeM (TicketSeatManagementNotFound serviceCatId.getId (show visitDate))
      QTSM.updateBlockedSeats (seatManagement.blocked - tbsc.bookedSeats) serviceCatId visitDate

postTicketBookingsUpdateSeats :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketBookingsUpdateSeats _ TicketBookingUpdateSeatsReq {..} = do
  mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate categoryId date
  now <- getCurrentTime
  if isJust mbSeatM
    then QTSM.updateBookedSeats updatedBookedSeats categoryId date
    else do
      seatId <- generateGUID
      let seatM =
            Domain.Types.SeatManagement.SeatManagement
              { id = seatId,
                ticketServiceCategoryId = categoryId,
                date = date,
                blocked = 0,
                booked = updatedBookedSeats,
                merchantId = Nothing,
                merchantOperatingCityId = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QTSM.create seatM
  pure Kernel.Types.APISuccess.Success
