module Domain.Action.UI.TicketService where

import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified API.Types.Dashboard.AppManagement.Tickets as Tickets
import API.Types.UI.TicketService
import qualified Crypto.Hash as Hash
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (partition)
import qualified Data.Map as Map
import Data.Ord as DO
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
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
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.Payment.Interface.Types as Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.Prelude as Kernel.Prelude
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import Lib.Payment.Domain.Types.Refunds (Refunds (..))
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.TicketRule.Apply as TicketRule
import qualified SharedLogic.TicketRule.Core
import SharedLogic.TicketUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.BusinessHour as QBH
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonExtra as PersonExtra
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
import Tools.Error
import qualified Tools.Notifications as Notifications
import qualified Tools.Payment as Payment
import qualified Tools.SMS as Sms

--import Control.Monad.Extra (mapMaybeM)

type TicketBookingServiceMap = Map.Map (Kernel.Types.Id.Id DTB.TicketBookingService) DTB.TicketBookingService

type TicketBookingServiceShortIdMap = Map.Map (Kernel.Types.Id.ShortId DTB.TicketBookingService) DTB.TicketBookingService

type TicketBookingMap = Map.Map (Kernel.Types.Id.Id DTTB.TicketBooking) DTTB.TicketBooking

type TicketBookingPeopleCategoryMap = Map.Map (Kernel.Types.Id.Id DTB.TicketBookingPeopleCategory) DTB.TicketBookingPeopleCategory

type TicketBookingServiceCategoryMap = Map.Map (Kernel.Types.Id.Id DTB.TicketBookingServiceCategory) DTB.TicketBookingServiceCategory

type TicketServiceMap = Map.Map (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService) Domain.Types.TicketService.TicketService

type PersonMap = Map.Map (Kernel.Types.Id.Id DP.Person) DP.Person

data ConvertedTime = ConvertedTime
  { slot :: Maybe Kernel.Prelude.TimeOfDay,
    startTime :: Maybe Kernel.Prelude.TimeOfDay,
    endTime :: Maybe Kernel.Prelude.TimeOfDay
  }

data CancellationInfo = CancellationInfo
  { cancelledServices :: [ServiceCancellationInfo],
    ticketbooking :: DTTB.TicketBooking
  }

data ServiceCancellationInfo = ServiceCancellationInfo
  { cancelledService :: DTB.TicketBookingService,
    cancelledServiceCategories :: [CategoryCancellationInfo]
  }

data CategoryCancellationInfo = CategoryCancellationInfo
  { cancelledCategory :: DTB.TicketBookingServiceCategory,
    cancelledPeopleCategories :: [PeopleCategoryCancellationInfo],
    businessHourId :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    serviceCategoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory
  }

data PeopleCategoryCancellationInfo = PeopleCategoryCancellationInfo
  { cancelledPeopleCategory :: DTB.TicketBookingPeopleCategory,
    cancelledQuantity :: Int
  }

convertBusinessHT :: Domain.Types.BusinessHour.BusinessHourType -> ConvertedTime
convertBusinessHT (Domain.Types.BusinessHour.Slot time) = ConvertedTime {slot = Just time, startTime = Nothing, endTime = Nothing}
convertBusinessHT (Domain.Types.BusinessHour.Duration startTime endTime) = ConvertedTime {slot = Nothing, startTime = Just startTime, endTime = Just endTime}

getTicketPlaces :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace]
getTicketPlaces (_, merchantId) = do
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId
  context <- TicketRule.getCurrentContext 330 Nothing Nothing
  ticketPlaces' <- QTP.getTicketPlaces merchantOpCity.id
  let ticketPlaces = TicketRule.processEntity context <$> ticketPlaces'
  pure $ sortBy (comparing (Down . (.priority))) $ filterEndedOrUnPublishedPlaces ticketPlaces
  where
    filterEndedOrUnPublishedPlaces = filter (\place -> place.status `notElem` [Domain.Types.TicketPlace.Ended, Domain.Types.TicketPlace.Unpublished])

getTicketPlacesServices :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
getTicketPlacesServices _ placeId mbDate = do
  ticketServices <- QTS.getTicketServicesByPlaceId placeId.getId
  now <- getCurrentTime
  let bookingDate = fromMaybe (utctDay now) mbDate
  context <- TicketRule.getCurrentContext 330 mbDate Nothing
  let ruledTicketServices = filter (\s -> not s.isClosed) $ (TicketRule.processEntity context) <$> ticketServices
  mkTicketServiceListRes ruledTicketServices bookingDate placeId
  where
    mkTicketServiceListRes ticketServices bookingDate_ pId =
      mapM
        ( \service -> do
            specialOccasions <- findSpecialOccasion service
            let getBusinessHourForSpecialLocation sl businessHours = if null businessHours then map (\bh -> (sl, bh)) service.businessHours else map (\bh -> (sl, bh)) businessHours
            let specialOccBHourIds = concat (map (\sl -> getBusinessHourForSpecialLocation sl sl.businessHours) specialOccasions)
            specialOccBHoursWithOverrides <- mapM (\(specialOcc, bhId) -> mkBusinessHoursRes service bookingDate_ (Just specialOcc) bhId) specialOccBHourIds
            let specialOccBHours = fst <$> specialOccBHoursWithOverrides
            normalBusinessHoursWithOverrides <- mapM (\bhId -> mkBusinessHoursRes service bookingDate_ Nothing bhId) service.businessHours
            let normalBusinessHours = fst <$> normalBusinessHoursWithOverrides
                overrideBusinessHours = snd <$> normalBusinessHoursWithOverrides
            mbNewServiceBusinessHourIds <- checkForBusinessHourOverrides service overrideBusinessHours
            newServiceBusinessHours <- case mbNewServiceBusinessHourIds of
              Just newServiceBusinessHourIds -> do
                bhs <- mapM (\bhId -> mkBusinessHoursRes service bookingDate_ Nothing bhId) newServiceBusinessHourIds
                return $ if null bhs then normalBusinessHours else fst <$> bhs
              Nothing -> return normalBusinessHours
            let businessHours = filter (\bh -> not $ null bh.categories) $ newServiceBusinessHours ++ specialOccBHours
            pure $
              TicketServiceResp
                { id = service.id,
                  placesId = pId,
                  name = service.service,
                  maxVerification = service.maxVerification,
                  allowFutureBooking = service.allowFutureBooking,
                  expiry = service.expiry,
                  businessHours,
                  shortDesc = service.shortDesc,
                  allowCancellation = service.allowCancellation
                }
        )
        ticketServices

    mkBusinessHoursRes ::
      Domain.Types.TicketService.TicketService ->
      Data.Time.Calendar.Day ->
      Maybe Domain.Types.SpecialOccasion.SpecialOccasion ->
      Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour ->
      Environment.Flow
        ( BusinessHourResp,
          ( [ ( Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
                Maybe [SharedLogic.TicketRule.Core.ActionType]
              )
            ],
            [ ( Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
                SharedLogic.TicketRule.Core.BusinessHourDef
              )
            ]
          )
        )
    mkBusinessHoursRes service bDate mbSpecialOcc bhId = do
      businessHour <- QBH.findById bhId >>= fromMaybeM (BusinessHourNotFound bhId.getId)
      let convertedBusinessHT = convertBusinessHT businessHour.btype
          mbOperationalDay = (.dayOfWeek) =<< mbSpecialOcc
      categoriesWithBHOverrides' <- mapM (mkServiceCategories bDate) businessHour.categoryId
      let categoriesWithBHOverrides = filter (\(c, _) -> not c.isClosed) categoriesWithBHOverrides'
      let categories = fst <$> categoriesWithBHOverrides
      let remainingBHOverrides = map (\(c, ov) -> (c.id, ov)) categoriesWithBHOverrides
      let svcWrtBhs =
            map
              ( \c ->
                  let coreBHDef = SharedLogic.TicketRule.Core.BusinessHourDef (convertDomainBHTtoCoreBHT businessHour.btype) businessHour.bookingClosingTime
                   in (c.id, coreBHDef)
              )
              categories
      pure $
        ( BusinessHourResp
            { id = bhId,
              slot = convertedBusinessHT.slot,
              startTime = convertedBusinessHT.startTime,
              endTime = convertedBusinessHT.endTime,
              specialDayDescription = (.description) =<< mbSpecialOcc,
              specialDayType = (.specialDayType) <$> mbSpecialOcc,
              operationalDays = maybe service.operationalDays (: []) mbOperationalDay,
              operationalDate = service.operationalDate,
              categories
            },
          (remainingBHOverrides, svcWrtBhs)
        )

    mkServiceCategories bDate_ serviceCatId = do
      context <- TicketRule.getCurrentContext 330 (Just bDate_) Nothing
      serviceCategory' <- QSC.findById serviceCatId >>= fromMaybeM (ServiceCategoryNotFound serviceCatId.getId)
      let serviceCategory = TicketRule.processEntity context serviceCategory'
      isClosed <-
        QSO.findBySplDayAndEntityIdAndDate Domain.Types.SpecialOccasion.Closed (serviceCatId.getId) (Just bDate_) >>= \case
          Just _ -> pure True
          Nothing -> pure False
      mBeatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId bDate_
      peopleCategories <- mapM (mkPeopleCategoriesRes bDate_) serviceCategory.peopleCategory
      pure $
        ( CategoriesResp
            { name = serviceCategory.name,
              id = serviceCategory.id,
              availableSeats = serviceCategory.availableSeats,
              bookedSeats = maybe 0 (.booked) mBeatManagement,
              allowedSeats = calcAllowedSeats serviceCategory mBeatManagement,
              peopleCategories,
              isClosed = isClosed || serviceCategory.isClosed || null peopleCategories
            },
          serviceCategory.remainingActions
        )

    mkPeopleCategoriesRes bDate_ pCatId = do
      context <- TicketRule.getCurrentContext 330 (Just bDate_) Nothing
      peopleCategory' <- QPC.findServicePeopleCategoryById pCatId bDate_ >>= fromMaybeM (PeopleCategoryNotFound pCatId.getId)
      let peopleCategory = TicketRule.processEntity context peopleCategory'
      pure $
        PeopleCategoriesResp
          { name = peopleCategory.name,
            id = peopleCategory.id,
            pricePerUnit = peopleCategory.pricePerUnit.amount,
            pricePerUnitWithCurrency = mkPriceAPIEntity peopleCategory.pricePerUnit,
            description = peopleCategory.description,
            cancellationCharges = peopleCategory.cancellationCharges
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

  -- Redis blocking starts here --
  -- setup if not done
  let requestedUnitsCount = calculateTotalRequestedUnits req
  forM_ (fst <$> requestedUnitsCount) (\svcCategoryId -> setupBlockMechanismNx svcCategoryId req.visitDate)

  -- try blocking seats for all and release if failed
  res <- blockSeats personId_ req.visitDate requestedUnitsCount
  mbBlockExpiryTime <- case res of BlockFailed msg -> throwError (InvalidRequest msg); BlockSuccess expTime -> return expTime

  ticketBookingId <- generateGUID
  ticketBookingServices <- mapM (createTicketBookingService merchantOpCity.id ticketBookingId req.visitDate) req.services

  amount <- withCurrencyCheckingList (ticketBookingServices <&> (.amount)) $ \mbCurrency as ->
    mkPrice mbCurrency $ sum as

  let bookedSeats = sum $ ticketBookingServices <&> (.bookedSeats)
      vendorSplits = accumulateVendorSplits (ticketBookingServices <&> (.vendorSplitDetails))

  ticketBooking <- createTicketBooking personId_ merchantOpCity.id ticketBookingId amount bookedSeats vendorSplits mbBlockExpiryTime

  QTBS.createMany ticketBookingServices
  QTB.create ticketBooking

  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOpCity.id (Just placeId) Payment.Normal
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = ticketBooking.id.getId,
            orderShortId = ticketBooking.shortId.getShortId,
            amount = amount.amount,
            customerId = personId_.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = personPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing,
            splitSettlementDetails = Payment.mkSplitSettlementDetails isSplitEnabled amount.amount (fromMaybe [] vendorSplits)
          }
  let commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId_
      createOrderCall = Payment.createOrder merchantId merchantOpCity.id (Just placeId) Payment.Normal (Just person.id.getId) person.clientSdkVersion
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast merchantOpCity.id) commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return createOrderRes
    Nothing -> do
      throwError $ InternalError "Failed to create order"
  where
    accumulateVendorSplits mbSplits = mbSplits & catMaybes & concat & Payment.groupSumVendorSplits & \l -> bool (pure l) (Nothing) (null l)
    createTicketBooking personId_ merchantOperatingCityId ticketBookingId amount bookedSeats vendorSplits mbBlockExpiryTime = do
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
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            bookedSeats,
            cancelledSeats = Nothing,
            vendorSplitDetails = vendorSplits,
            blockExpirationTime = mbBlockExpiryTime
          }

    createTicketBookingService merchantOperatingCityId ticketBookingId visitDate ticketServicesReq = do
      let ticketServiceId = ticketServicesReq.serviceId
      let bHourId = ticketServicesReq.businessHourId
      let categories = ticketServicesReq.categories
      context <- TicketRule.getCurrentContext 330 (Just visitDate) Nothing
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      ticketService' <- QTS.findById ticketServicesReq.serviceId >>= fromMaybeM (TicketServiceNotFound ticketServicesReq.serviceId.getId)
      let ticketService = TicketRule.processEntity context ticketService'
      businessHour <- QBH.findById bHourId >>= fromMaybeM (BusinessHourNotFound bHourId.getId)

      let businessHourTime = case businessHour.btype of
            Domain.Types.BusinessHour.Slot time -> time
            Domain.Types.BusinessHour.Duration _ endTime' -> endTime'
      let visitDateTime = UTCTime visitDate (timeOfDayToTime businessHourTime)
      case businessHour.bookingClosingTime of
        Just closingTime ->
          when (UTCTime visitDate (timeOfDayToTime closingTime) < now) $ throwError $ InvalidRequest "Booking is Closed Now" -- Normal flow
        Nothing -> return ()
      when (visitDateTime < now) $ throwError $ InvalidRequest "Cannot book for past date"

      tBookingSCats <- mapM (createTicketBookingServiceCategory merchantOperatingCityId id visitDate businessHour) categories
      QTBSC.createMany tBookingSCats
      amount <- withCurrencyCheckingList (tBookingSCats <&> (.amount)) $ \mbCurrency as ->
        mkPrice mbCurrency $ sum as
      let bookedSeats = sum $ tBookingSCats <&> (.bookedSeats)
      let expiry = calcExpiry ticketService.expiry req.visitDate now
      return $
        DTB.TicketBookingService
          { id,
            shortId,
            ticketBookingId,
            ticketServiceId,
            amount,
            btype = businessHour.btype,
            bHourId = Just businessHour.id,
            status = DTB.Pending,
            verificationCount = 0,
            expiryDate = Just expiry,
            merchantId = Just merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            visitDate = Just visitDate,
            bookedSeats,
            cancelledSeats = Nothing,
            vendorSplitDetails = accumulateVendorSplits ((.vendorSplitDetails) <$> tBookingSCats)
          }

    createTicketBookingServiceCategory merchantOperatingCityId ticketBookingServiceId visitDate businessHour ticketServiceCReq = do
      id <- generateGUID
      now <- getCurrentTime
      context <- TicketRule.getCurrentContext 330 (Just visitDate) Nothing
      mbAnySplOccassion <- QSO.findBySplDayAndEntityIdAndDate Domain.Types.SpecialOccasion.Closed (ticketServiceCReq.categoryId.getId) (Just visitDate)
      when (maybe False (\anySplOccassion -> elem businessHour.id anySplOccassion.businessHours) mbAnySplOccassion) $ throwError $ InvalidRequest "Business hour is closed"
      let serviceCatId = ticketServiceCReq.categoryId
      tBookingSC' <- QSC.findById serviceCatId >>= fromMaybeM (ServiceCategoryNotFound serviceCatId.getId)
      let tBookingSC = TicketRule.processEntity context tBookingSC'
      tBookingPCats <- mapM (createTicketBookingPeopleCategory now merchantOperatingCityId id visitDate) ticketServiceCReq.peopleCategories
      (amount, bookedSeats) <- calculateAmountAndSeats tBookingPCats
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
                    maxCapacity = tBookingSC.availableSeats,
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
            updatedAt = now,
            cancelledSeats = Nothing,
            eventCancelledBy = Nothing,
            amountToRefund = Nothing,
            visitDate = Just visitDate,
            btype = Just businessHour.btype,
            vendorSplitDetails = accumulateVendorSplits ((.vendorSplitDetails) <$> tBookingPCats)
          }

    createTicketBookingPeopleCategory now merchantOperatingCityId ticketBookingServiceCategoryId visitDate ticketServicePCReq = do
      id <- generateGUID
      let tPCatId = ticketServicePCReq.peopleCategoryId
      context <- TicketRule.getCurrentContext 330 (Just visitDate) Nothing
      tServicePCat' <- QPC.findServicePeopleCategoryById tPCatId visitDate >>= fromMaybeM (PeopleCategoryNotFound tPCatId.getId)
      let tServicePCat = TicketRule.processEntity context tServicePCat'
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
            numberOfUnitsCancelled = Nothing,
            amountToRefund = Nothing,
            createdAt = now,
            updatedAt = now,
            peopleCategoryId = Just tPCatId,
            vendorSplitDetails = (map (multiplyVendorSplits numberOfUnits)) <$> tServicePCat.vendorSplitDetails
          }

    multiplyVendorSplits :: Int -> Payment.VendorSplitDetails -> Payment.VendorSplitDetails
    multiplyVendorSplits count vendorSplitDetail = vendorSplitDetail {Payment.splitAmount = Payment.roundToTwoDecimalPlaces (vendorSplitDetail.splitAmount * (Payment.roundToTwoDecimalPlaces $ fromIntegral count))}

    calculateAmountAndSeats :: (MonadThrow m, Log m) => [DTB.TicketBookingPeopleCategory] -> m (Price, Int)
    calculateAmountAndSeats categories = do
      let categoriesNumberOfUnits = categories <&> (.numberOfUnits)
      withCurrencyCheckingList (categories <&> (.pricePerUnit)) $ \mbCurrency categoriesPricePerUnit -> do
        first (mkPrice mbCurrency) $
          foldl'
            ( \(totalAmount, totalSeats) (pricePerUnit, numberOfUnits) -> do
                let categoryAmount = pricePerUnit * fromIntegral numberOfUnits
                (totalAmount + categoryAmount, totalSeats + numberOfUnits)
            )
            (HighPrecMoney 0, 0)
            (zip categoriesPricePerUnit categoriesNumberOfUnits)

    calcExpiry :: Domain.Types.TicketService.ExpiryType -> Data.Time.Calendar.Day -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime
    calcExpiry expiry visitDate currentTime = case expiry of
      Domain.Types.TicketService.InstantExpiry minutes -> addUTCTime (fromIntegral (minutes * 60)) currentTime
      Domain.Types.TicketService.VisitDate timeOfDay -> UTCTime visitDate (timeOfDayToTime timeOfDay)

getTicketBookings :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.TicketBooking.BookingStatus -> Environment.Flow [API.Types.UI.TicketService.TicketBookingAPIEntity]
getTicketBookings (mbPersonId, merchantId_) mbLimit mbOffset status_ = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId_
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
            amount = amount.amount,
            amountWithCurrency = mkPriceAPIEntity amount,
            ..
          }

getTicketBookingsDetails :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow API.Types.UI.TicketService.TicketBookingDetails
getTicketBookingsDetails (_mbPersonId, merchantId') shortId_ = do
  ticketBooking <- QTB.findByShortId shortId_ >>= fromMaybeM (TicketBookingNotFound shortId_.getShortId)
  ticketBookingServices <- QTBS.findAllByBookingId ticketBooking.id
  services <- mapM mkTicketBookingServiceDetails ticketBookingServices
  mkTicketBookingDetails ticketBooking services
  where
    mkTicketBookingDetails DTTB.TicketBooking {..} services = do
      refunds <- QRefunds.findAllByOrderId $ Kernel.Types.Id.Id shortId.getShortId
      person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
      let isAnyRefundPending = any (\refund -> refund.status == Kernel.External.Payment.Interface.Types.REFUND_PENDING) refunds
      refundDetails <-
        if isAnyRefundPending
          then do
            let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId
                orderStatusCall = Payment.orderStatus merchantId' merchantOperatingCityId (Just ticketPlaceId) Payment.Normal (Just person.id.getId) person.clientSdkVersion
            paymentStatus <- DPayment.orderStatusService commonPersonId (Kernel.Types.Id.Id id.getId) orderStatusCall
            mapM (mkRefundDetails shortId merchantId') paymentStatus.refunds
          else pure refunds

      ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (TicketPlaceNotFound ticketPlaceId.getId)
      return $
        TicketBookingDetails
          { ticketShortId = shortId.getShortId,
            ticketPlaceId = ticketPlaceId.getId,
            personId = personId.getId,
            ticketPlaceName = ticketPlace.name,
            amount = amount.amount,
            amountWithCurrency = mkPriceAPIEntity amount,
            refundDetails,
            ..
          }

    mkTicketBookingServiceDetails :: DTB.TicketBookingService -> Environment.Flow API.Types.UI.TicketService.TicketBookingServiceDetails
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
            businessHourId = bHourId,
            amount = amount.amount,
            amountWithCurrency = mkPriceAPIEntity amount,
            allowCancellation = ticketService.allowCancellation,
            ..
          }
    mkTicketBookingCategoryDetails :: DTB.TicketBookingServiceCategory -> Environment.Flow API.Types.UI.TicketService.TicketBookingCategoryDetails
    mkTicketBookingCategoryDetails DTB.TicketBookingServiceCategory {..} = do
      localTime <- getLocalCurrentTime 19800
      let visitDate_ = fromMaybe (utctDay localTime) visitDate
      peopleCategories <- QTBPC.findAllByServiceCategoryId id
      peopleCategoryDetails <- mapM (mkTicketBookingPeopleCategoryDetails visitDate_) peopleCategories
      return $
        TicketBookingCategoryDetails
          { peopleCategories = peopleCategoryDetails,
            amount = amount.amount,
            amountWithCurrency = mkPriceAPIEntity amount,
            amountToRefund = amountToRefund,
            bookedSeats = bookedSeats,
            ..
          }
    mkTicketBookingPeopleCategoryDetails :: Day -> DTB.TicketBookingPeopleCategory -> Environment.Flow API.Types.UI.TicketService.TicketBookingPeopleCategoryDetails
    mkTicketBookingPeopleCategoryDetails visitDate DTB.TicketBookingPeopleCategory {..} = do
      cancellationCharges <- case peopleCategoryId of
        Nothing -> pure Nothing
        Just peopleCategoryId' -> do
          peopleCategory' <- QPC.findServicePeopleCategoryById peopleCategoryId' visitDate
          pure $ (.cancellationCharges) =<< peopleCategory'
      return $
        TicketBookingPeopleCategoryDetails
          { pricePerUnit = pricePerUnit.amount,
            pricePerUnitWithCurrency = mkPriceAPIEntity pricePerUnit,
            cancelCharges = cancellationCharges,
            ..
          }
    mkRefundDetails :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Payment.RefundsData -> Environment.Flow Refunds
    mkRefundDetails orderId merchantId Payment.RefundsData {..} = do
      now <- getCurrentTime
      return $
        Refunds
          { id = Kernel.Types.Id.Id requestId,
            merchantId = merchantId.getId,
            shortId = requestId,
            status = status,
            orderId = Kernel.Types.Id.Id orderId.getShortId,
            refundAmount = amount,
            idAssignedByServiceProvider = Nothing,
            initiatedBy = Nothing,
            createdAt = now,
            updatedAt = now,
            ..
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
        DTB.Cancelled -> createVerificationResp CancelledBooking (Just bookingService) (Just ticketService) (Just booking)

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
      serviceCategories <- traverse QTBSC.findAllByTicketBookingServiceId mbTicketServiceId
      serviceCatDetails <- mkTicketBookingCategoryDetails serviceCategories
      pure $
        TicketServiceVerificationResp
          { ticketServiceName = mbTicketService <&> (.service),
            visitDate = mbBooking <&> (.visitDate),
            validTill = mbBookingService >>= (.expiryDate) >>= (Just . addUTCTime (secondsToNominalDiffTime 19800)), -- 19800 for +5:30 timezone
            ticketServiceShortId = mbBookingService <&> (.shortId) <&> (.getShortId),
            message = verificationMsg status,
            status,
            amount = mbBookingService <&> (.amount.amount),
            amountWithCurrency = mkPriceAPIEntity . (.amount) <$> mbBookingService,
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
                    { id = category.id,
                      peopleCategories = peopleCategoryDetails,
                      amount = category.amount.amount,
                      amountWithCurrency = mkPriceAPIEntity category.amount,
                      bookedSeats = category.bookedSeats - fromMaybe 0 category.cancelledSeats, -- for verify we should send booked seats - cancelled seats
                      name = category.name,
                      cancelledSeats = category.cancelledSeats,
                      amountToRefund = category.amountToRefund,
                      serviceCategoryId = category.serviceCategoryId
                    }
            )
            list

    mkTicketBookingPeopleCategoryDetails :: DTB.TicketBookingPeopleCategory -> TicketBookingPeopleCategoryDetails
    mkTicketBookingPeopleCategoryDetails DTB.TicketBookingPeopleCategory {..} =
      TicketBookingPeopleCategoryDetails
        { pricePerUnit = pricePerUnit.amount,
          pricePerUnitWithCurrency = mkPriceAPIEntity pricePerUnit,
          cancelCharges = Nothing,
          ..
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

    verificationMsg :: TicketVerificationStatus -> T.Text
    verificationMsg BookingSuccess = "Validated successfully!"
    verificationMsg BookingExpired = "Booking Expired!"
    verificationMsg BookingFuture = "Booking for Later Date!"
    verificationMsg BookingAlreadyVerified = "Already Validated!"
    verificationMsg DifferentService = "Different Service!"
    verificationMsg PaymentPending = "Payment Pending!"
    verificationMsg InvalidBooking = "Not a valid QR"
    verificationMsg CancelledBooking = "Booking Cancelled!"

getTicketBookingsStatus :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow Domain.Types.TicketBooking.BookingStatus
getTicketBookingsStatus (mbPersonId, merchantId) _shortId@(Kernel.Types.Id.ShortId shortId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  ticketBooking' <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId)
  let commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId ticketBooking'.merchantOperatingCityId (Just ticketBooking'.ticketPlaceId) Payment.Normal (Just person.id.getId) person.clientSdkVersion -- api call
  order <- QOrder.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (PaymentOrderNotFound shortId)
  ticketBookingServices <- QTBS.findAllByBookingId ticketBooking'.id
  tBookingServiceCats <- mapM (\tBookingS -> QTBSC.findAllByTicketBookingServiceId tBookingS.id) ticketBookingServices
  let ticketBookingServiceCategories = concat tBookingServiceCats
  let totalRefundAmount = Payment.roundToTwoDecimalPlaces ticketBooking'.amount.amount
  if ticketBooking'.status == DTTB.Cancelled || ticketBooking'.status == DTTB.Booked || ticketBooking'.status == DTTB.RefundInitiated
    then do
      return ticketBooking'.status
    else do
      paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
      case paymentStatus of
        DPayment.PaymentStatus {..} -> do
          when (status == Payment.CHARGED) $ do
            -- checking here if blockExpiryTime is passed
            currentTimeWithBuffer <- (10000 +) <$> getCurrentTimestamp
            let windowTimePassed = maybe False (\expTime -> currentTimeWithBuffer > expTime) ticketBooking'.blockExpirationTime
            res <-
              if windowTimePassed
                then tryInstantBooking personId ticketBookingServiceCategories
                else do
                  case ticketBooking'.blockExpirationTime of
                    Just _ -> tryLockBooking personId ticketBookingServiceCategories
                    Nothing -> return (LockBookingSuccess Nothing)
            case res of
              LockBookingSuccess _ -> do
                QTB.updateStatusByShortId DTTB.Booked _shortId
                QTBS.updateAllStatusByBookingId DTB.Confirmed ticketBooking'.id
                mapM_
                  ( \tbsc ->
                      whenJust tbsc.serviceCategoryId $ \serviceId ->
                        updateBookedAndBlockedSeats (Kernel.Types.Id.Id serviceId) tbsc ticketBooking'.visitDate
                  )
                  ticketBookingServiceCategories
              LockBookingFailed -> do
                intializeRefundProcess ticketBooking'.shortId (Just ticketBooking'.ticketPlaceId) totalRefundAmount merchantId ticketBooking'.merchantOperatingCityId (Just person.id.getId) person.clientSdkVersion
                QTB.updateStatusByShortId DTTB.RefundInitiated _shortId
                QTBS.updateAllStatusByBookingId DTB.Failed ticketBooking'.id
          when (status `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]) $ do
            QTB.updateStatusByShortId DTTB.Failed _shortId
            QTBS.updateAllStatusByBookingId DTB.Failed ticketBooking'.id
            mapM_
              ( \tbsc ->
                  whenJust tbsc.serviceCategoryId $ \serviceId ->
                    updateBlockedSeats (Kernel.Types.Id.Id serviceId) tbsc ticketBooking'.visitDate
              )
              ticketBookingServiceCategories
        _ -> pure ()

      ticketBooking <- QTB.findByShortId (Kernel.Types.Id.ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId) -- fetch again for updated status
      return ticketBooking.status
  where
    updateBookedAndBlockedSeats :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> DTB.TicketBookingServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow ()
    updateBookedAndBlockedSeats serviceCatId tbsc visitDate = do
      seatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId visitDate >>= fromMaybeM (TicketSeatManagementNotFound serviceCatId.getId (show visitDate))
      QTSM.updateBlockedSeats (seatManagement.blocked - tbsc.bookedSeats) serviceCatId visitDate
      QTSM.safeUpdateBookedSeats (seatManagement.booked + tbsc.bookedSeats) serviceCatId visitDate

    updateBlockedSeats :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> DTB.TicketBookingServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow ()
    updateBlockedSeats serviceCatId tbsc visitDate = do
      seatManagement <- QTSM.findByTicketServiceCategoryIdAndDate serviceCatId visitDate >>= fromMaybeM (TicketSeatManagementNotFound serviceCatId.getId (show visitDate))
      QTSM.updateBlockedSeats (seatManagement.blocked - tbsc.bookedSeats) serviceCatId visitDate

postTicketBookingsUpdateSeats :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketBookingsUpdateSeats _ TicketBookingUpdateSeatsReq {..} = do
  -- TODO: Here we need to make redis compatible update
  mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate categoryId date
  tBookingSC <- QSC.findById categoryId >>= fromMaybeM (InvalidRequest "TicketBookingServiceCategory not found")
  when (isNothing mbSeatM) $ do
    seatId <- generateGUID
    now <- getCurrentTime
    let seatM =
          Domain.Types.SeatManagement.SeatManagement
            { id = seatId,
              ticketServiceCategoryId = categoryId,
              date = date,
              blocked = 0,
              booked = 0,
              maxCapacity = tBookingSC.availableSeats,
              merchantId = Nothing,
              merchantOperatingCityId = Nothing,
              createdAt = now,
              updatedAt = now
            }
    QTSM.create seatM
  setupBlockMechanismNx categoryId date
  tryChangeBookedSeats categoryId updatedBookedSeats date
  pure Kernel.Types.APISuccess.Success

postTicketServiceCancel :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.TicketService.TicketServiceCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketServiceCancel (_mbPersonId, merchantId) req = do
  now <- getCurrentTime
  bHoursRes <- QBH.findById req.businessHourId >>= fromMaybeM (BusinessHourNotFound req.businessHourId.getId)
  fork "cancel and refund:" $ do
    isLockAcquired <- tryMerchantTicketCancellationLock req.businessHourId req.date req.ticketServiceCategoryId
    unless isLockAcquired $ throwError $ InvalidRequest "This Service Category Ticket cancellation is already in progress"
    preventFutureTicketBookingServiceStatus req.date req.description Domain.Types.SpecialOccasion.Closed req.ticketServiceCategoryId merchantId

    ticketBookingServices <- QTBS.findByVisitDateAndStatusAndServiceIdAndBtype (Just req.date) DTB.Confirmed req.ticketServiceId bHoursRes.btype (Just now)
    let tickekBookingServiceMap = Map.fromList $ (\tbs -> (tbs.id, tbs)) <$> ticketBookingServices

    ticketBookings <- QTB.findByIds $ (.ticketBookingId) <$> ticketBookingServices
    let ticketBookingMap = Map.fromList $ (\tb -> (tb.id, tb)) <$> ticketBookings

    ticketBookingServiceCategory <- QTBSC.findAllByServiceCategoryIdDateAndBtype (Just req.ticketServiceCategoryId.getId) (Just req.date) (Just bHoursRes.btype)

    let personIds = foldr (\tbs personIds' -> maybe personIds' (\tb -> tb.personId : personIds') (Map.lookup tbs.ticketBookingId ticketBookingMap)) [] ticketBookingServices
    person <- PersonExtra.findAllByIds personIds
    let personMap = Map.fromList $ (\per -> (per.id, per)) <$> person

    refundAndNotificationCalls <- mapM (cancelAndRefundConfirmedBookingService personMap tickekBookingServiceMap ticketBookingMap) ticketBookingServiceCategory
    fork "refund api calls" $ mapM_ fst refundAndNotificationCalls
    fork "notification api calls" $ mapM_ snd refundAndNotificationCalls

  pure Kernel.Types.APISuccess.Success
  where
    preventFutureTicketBookingServiceStatus :: Data.Time.Calendar.Day -> Maybe Text -> Domain.Types.SpecialOccasion.SpecialDayType -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Environment.Flow ()
    preventFutureTicketBookingServiceStatus visitDate cancelDescription specialDayType serviceCategoryId mId = do
      QTSM.updateBookedAndBlockedSeats 0 0 serviceCategoryId visitDate
      mbSplOccasion <- QSO.findBySplDayAndEntityIdAndDate Domain.Types.SpecialOccasion.Closed serviceCategoryId.getId (Just visitDate)
      case mbSplOccasion of
        Nothing -> mkSpecialOccasionEntry req.businessHourId (Just visitDate) cancelDescription serviceCategoryId specialDayType (Just mId)
        Just splOccasion -> unless (elem req.businessHourId splOccasion.businessHours) $ QSO.updateBusinessHoursById (splOccasion.businessHours ++ [req.businessHourId]) splOccasion.id

    mkSpecialOccasionEntry :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Maybe Data.Time.Calendar.Day -> Maybe Text -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Domain.Types.SpecialOccasion.SpecialDayType -> Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow ()
    mkSpecialOccasionEntry bHourId date description serviceCategoryId specialDayType mbMerchantId = do
      splOccasionId <- generateGUID
      now <- getCurrentTime
      let splOccasionEntry =
            Domain.Types.SpecialOccasion.SpecialOccasion
              { businessHours = [bHourId],
                date = date,
                dayOfWeek = Nothing,
                description = description,
                entityId = serviceCategoryId.getId,
                id = splOccasionId,
                specialDayType = specialDayType,
                merchantId = mbMerchantId,
                merchantOperatingCityId = Nothing,
                name = Nothing,
                placeId = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QSO.create splOccasionEntry

    cancelAndRefundConfirmedBookingService :: PersonMap -> TicketBookingServiceMap -> TicketBookingMap -> DTB.TicketBookingServiceCategory -> Environment.Flow (Environment.Flow (), Environment.Flow ())
    cancelAndRefundConfirmedBookingService personMap ticketBookingServiceMap ticketBookingMap tbServiceCategory = do
      let ticketBookingService' = Map.lookup tbServiceCategory.ticketBookingServiceId ticketBookingServiceMap
          ticketBooking' = ticketBookingService' >>= (\tbs -> Map.lookup tbs.ticketBookingId ticketBookingMap)

      case (ticketBooking', ticketBookingService') of
        (Just ticketBooking, Just ticketBookingService) -> do
          let refundAmount = tbServiceCategory.amount.amount - fromMaybe (HighPrecMoney 0) tbServiceCategory.amountToRefund -- should we refund all amount if user had cancelled some ticket then merchant had cancelled whole category
          void $ QTBSC.updateCancellationDetailsById (Just tbServiceCategory.bookedSeats) (Just tbServiceCategory.amount.amount) (Just DTB.Merchant) tbServiceCategory.id
          updateStatusForBooking ticketBooking ticketBookingService tbServiceCategory.bookedSeats
          case Map.lookup ticketBooking.personId personMap of
            Just person -> do
              let refundCall = when (refundAmount > 0) $ intializeRefundProcess ticketBooking.shortId (Just ticketBooking.ticketPlaceId) refundAmount person.merchantId person.merchantOperatingCityId (Just person.id.getId) person.clientSdkVersion
              let notificationCall = Notifications.notifyTicketCancelled ticketBooking.id.getId tbServiceCategory.name person
              pure (refundCall, notificationCall)
            Nothing -> pure (unit, unit)
        (_, _) -> pure (unit, unit)
      where
        unit = pure ()
        updateStatusForBooking :: DTTB.TicketBooking -> DTB.TicketBookingService -> Int -> Environment.Flow ()
        updateStatusForBooking ticketBooking ticketBookingService cancelledSeats = do
          let tbsCancelledSeats = fromMaybe 0 ticketBookingService.cancelledSeats + cancelledSeats
          let updateTicketBookingServiceStatus = if tbsCancelledSeats == ticketBookingService.bookedSeats then DTB.Cancelled else ticketBookingService.status
          QTBS.updateStatusAndCancelledSeatsById updateTicketBookingServiceStatus (Just tbsCancelledSeats) ticketBookingService.id
          let tbCancelledSeats = fromMaybe 0 ticketBooking.cancelledSeats + cancelledSeats
          let updateTicketBookingStatus = if tbCancelledSeats == ticketBooking.bookedSeats then DTTB.Cancelled else ticketBooking.status
          QTB.updateStatusAndCancelledSeatsById updateTicketBookingStatus (Just tbCancelledSeats) ticketBooking.id

    _sendSms :: Domain.Types.Person.Person -> Text -> Environment.Flow ()
    _sendSms person categoryName = do
      mobileNumber <- mapM decrypt person.mobileNumber
      let countryCode = fromMaybe "+91" person.mobileCountryCode
          merchantOperatingCityId = person.merchantOperatingCityId
      case mobileNumber of
        Just mNumber -> do
          let phoneNumber = countryCode <> mNumber
          buildSmsReq <- MessageBuilder.buildTicketBookingCancelled merchantOperatingCityId $ MessageBuilder.BuildTicketBookingCancelledMessageReq {personName = fromMaybe "" person.firstName, categoryName = categoryName}
          Sms.sendSMS person.merchantId merchantOperatingCityId (buildSmsReq phoneNumber)
            >>= Sms.checkSmsResult
        _ -> logDebug $ "Could n't send Ticket Booking Cancellation SMS : " <> person.id.getId

postTicketBookingCancel :: (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.TicketService.TicketBookingCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketBookingCancel (mbPersonId, merchantId) req = do
  let lockKeys = makeUserLockKeyTuple req
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  executeNLockOperations lockKeys $ do
    ticketBooking <- QTB.findByShortId req.ticketBookingShortId >>= fromMaybeM (TicketBookingNotFound req.ticketBookingShortId.getShortId)
    ticketBookingServices <- QTBS.findByShortIds $ (.shortId) <$> req.ticketBookingServices
    ticketService <- QTS.findByIds $ (.ticketServiceId) <$> ticketBookingServices
    ticketBookingServiceCategory <- QTBSC.findByIds $ concatMap (\serviceReq -> (.id) <$> serviceReq.serviceCategory) req.ticketBookingServices
    ticketBookingPeopleCategory <- QTBPC.findByIds $ concatMap (\serviceReq -> concatMap (\serviceCategory -> (.id) <$> serviceCategory.peopleCategory) serviceReq.serviceCategory) req.ticketBookingServices
    let ticketBookingServiceMap = Map.fromList $ (\tbs -> (tbs.shortId, tbs)) <$> ticketBookingServices
    let ticketServiceMap = Map.fromList $ (\ts -> (ts.id, ts)) <$> ticketService
    let ticketBookingServiceCategoryMap = Map.fromList $ (\tbsc -> (tbsc.id, tbsc)) <$> ticketBookingServiceCategory
    let ticketBookingPeopleCategoryMap = Map.fromList $ (\tbpc -> (tbpc.id, tbpc)) <$> ticketBookingPeopleCategory
    validationResp <- validateCancellationRequest ticketBooking req ticketBookingServiceMap ticketServiceMap ticketBookingServiceCategoryMap ticketBookingPeopleCategoryMap

    resp <- mapM (cancelTicketBookingService ticketBooking) validationResp.cancelledServices
    let totalRefundAmount = sum $ fst <$> resp
    let totalNoOfCancelledSeats = sum (snd <$> resp) + fromMaybe 0 ticketBooking.cancelledSeats
    let status = if totalNoOfCancelledSeats == ticketBooking.bookedSeats then DTTB.Cancelled else ticketBooking.status

    QTB.updateStatusAndCancelledSeatsById status (Just totalNoOfCancelledSeats) ticketBooking.id

    when (totalRefundAmount > 0) $ intializeRefundProcess ticketBooking.shortId (Just ticketBooking.ticketPlaceId) totalRefundAmount merchantId ticketBooking.merchantOperatingCityId (Just person.id.getId) person.clientSdkVersion

  pure Kernel.Types.APISuccess.Success
  where
    makeUserLockKeyTuple :: API.Types.UI.TicketService.TicketBookingCancelReq -> [(Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour, Data.Time.Calendar.Day, Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory)]
    makeUserLockKeyTuple req' = concatMap (\serviceReq -> (\category -> (serviceReq.businessHourId, category.visitDate, category.serviceCategoryId)) <$> serviceReq.serviceCategory) req'.ticketBookingServices

    executeNLockOperations :: [(Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour, Data.Time.Calendar.Day, Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory)] -> Environment.Flow () -> Environment.Flow ()
    executeNLockOperations [] cancelAndRefundOperation = cancelAndRefundOperation
    executeNLockOperations ((businessHourId, visitDate, serviceCategoryId) : lockKeys) cancelAndRefundOperation = do
      tryUserTicketCancellationLock businessHourId visitDate serviceCategoryId $
        executeNLockOperations lockKeys cancelAndRefundOperation

    validateCancellationRequest :: DTTB.TicketBooking -> API.Types.UI.TicketService.TicketBookingCancelReq -> TicketBookingServiceShortIdMap -> TicketServiceMap -> TicketBookingServiceCategoryMap -> TicketBookingPeopleCategoryMap -> Environment.Flow CancellationInfo
    validateCancellationRequest tb req' ticketBookingServiceMap ticketServiceMap ticketBookingCategoryMap ticketBookingPeopleCategoryMap = do
      unless (tb.status == DTTB.Booked) $
        throwError $ TicketBookingNotConfirmed (tb.shortId.getShortId)
      cancelledServices <- forM (req'.ticketBookingServices) $ \tbServiceReq -> do
        validateTicketBookingService tb tbServiceReq ticketBookingServiceMap ticketServiceMap ticketBookingCategoryMap ticketBookingPeopleCategoryMap
      return $ CancellationInfo {ticketbooking = tb, cancelledServices = cancelledServices}

    validateTicketBookingService :: DTTB.TicketBooking -> API.Types.UI.TicketService.TicketBookingServiceCancelReq -> TicketBookingServiceShortIdMap -> TicketServiceMap -> TicketBookingServiceCategoryMap -> TicketBookingPeopleCategoryMap -> Environment.Flow ServiceCancellationInfo
    validateTicketBookingService tb tbServiceReq ticketBookingServiceMap ticketServiceMap ticketBookingCategoryMap ticketBookingPeopleCategoryMap = do
      currentTime <- getCurrentTime
      tbService <- fromMaybeM (TicketBookingServiceNotFound (tbServiceReq.shortId.getShortId)) (Map.lookup (tbServiceReq.shortId) ticketBookingServiceMap)
      ticketService <- fromMaybeM (TicketBookingServiceNotFound (tbServiceReq.shortId.getShortId)) (Map.lookup tbService.ticketServiceId ticketServiceMap)

      unless (tbService.ticketBookingId == tb.id) $
        throwError $ InvalidRequest "The ticket booking service does not belong to this booking."
      unless (tbService.status == DTB.Confirmed && ticketService.allowCancellation) $
        throwError $ InvalidRequest "Ticket Cancellation Not allowed"
      unless (maybe True (currentTime <) tbService.expiryDate) $
        throwError $ InvalidRequest "Ticket Cancellation Expired"
      cancelledServiceCategories <- forM (tbServiceReq.serviceCategory) $ \serviceCategoryReq -> do
        validateTicketBookingServiceCategory tbService serviceCategoryReq ticketBookingCategoryMap ticketBookingPeopleCategoryMap
      return $ ServiceCancellationInfo {cancelledService = tbService, cancelledServiceCategories}

    validateTicketBookingServiceCategory :: DTB.TicketBookingService -> API.Types.UI.TicketService.TicketBookingServiceCategoryCancelReq -> TicketBookingServiceCategoryMap -> TicketBookingPeopleCategoryMap -> Environment.Flow CategoryCancellationInfo
    validateTicketBookingServiceCategory tbService serviceCategoryReq ticketBookingCategoryMap ticketBookingPeopleCategoryMap = do
      tbCategory <- fromMaybeM (ServiceCategoryNotFound serviceCategoryReq.id.getId) (Map.lookup (serviceCategoryReq.id) ticketBookingCategoryMap)
      businessHourId <- fromMaybeM (BusinessHourNotFound "Business Hour Not Found") tbService.bHourId
      serviceCategoryId <- pure tbCategory.serviceCategoryId >>= fromMaybeM (ServiceCategoryNotFound $ "Service Category not present in " <> tbCategory.id.getId)
      unless (tbCategory.ticketBookingServiceId == tbService.id) $
        throwError $ InvalidRequest "The ticket booking service category does not belong to this service."
      when (isJust tbCategory.eventCancelledBy) $
        throwError $ InvalidRequest "Event is already cancelled"
      cancelledPeopleCategories <- forM (serviceCategoryReq.peopleCategory) $ \peopleCategory -> do
        validateTicketBookingPeopleCategory tbCategory peopleCategory ticketBookingPeopleCategoryMap
      return $ CategoryCancellationInfo {cancelledCategory = tbCategory, cancelledPeopleCategories, businessHourId = businessHourId, serviceCategoryId = Kernel.Types.Id.Id serviceCategoryId}

    validateTicketBookingPeopleCategory :: DTB.TicketBookingServiceCategory -> API.Types.UI.TicketService.TicketBookingServicePeopleCategoryCancelReq -> TicketBookingPeopleCategoryMap -> Environment.Flow PeopleCategoryCancellationInfo
    validateTicketBookingPeopleCategory tbCategory peopleCategory ticketBookingPeopleCategoryMap = do
      tbPeopleCategory <- fromMaybeM (PeopleCategoryNotFound peopleCategory.id.getId) (Map.lookup peopleCategory.id ticketBookingPeopleCategoryMap)
      unless (tbPeopleCategory.ticketBookingServiceCategoryId == tbCategory.id) $
        throwError $ InvalidRequest "The ticket booking service people category does not belong to this service category."
      when (tbPeopleCategory.numberOfUnits - fromMaybe 0 tbPeopleCategory.numberOfUnitsCancelled < peopleCategory.quantity) $
        throwError $ InvalidRequest "Invalid Quantity"
      when (peopleCategory.quantity <= 0) $
        throwError $ InvalidRequest "Invalid Quantity"
      return $ PeopleCategoryCancellationInfo {cancelledPeopleCategory = tbPeopleCategory, cancelledQuantity = peopleCategory.quantity}

cancelTicketBookingService :: DTTB.TicketBooking -> ServiceCancellationInfo -> Environment.Flow (HighPrecMoney, Int)
cancelTicketBookingService ticketBooking ServiceCancellationInfo {..} = do
  let startTime = case cancelledService.btype of
        Domain.Types.BusinessHour.Slot time -> time
        Domain.Types.BusinessHour.Duration startTime' _ -> startTime'
  refundAmountAndCancelledSeatRes <- mapM (cancelTBServiceCategory ticketBooking.visitDate startTime) cancelledServiceCategories
  let totalRefundAmount = sum $ fst <$> refundAmountAndCancelledSeatRes
  let noOfCancelledSeats = sum $ snd <$> refundAmountAndCancelledSeatRes
  let tbsStatus = if (fromMaybe 0 cancelledService.cancelledSeats) + noOfCancelledSeats == cancelledService.bookedSeats then DTB.Cancelled else cancelledService.status
  QTBS.updateStatusAndCancelledSeatsById tbsStatus (Just $ (fromMaybe 0 cancelledService.cancelledSeats) + noOfCancelledSeats) cancelledService.id
  pure (totalRefundAmount, noOfCancelledSeats)

cancelTBServiceCategory :: Data.Time.Calendar.Day -> Kernel.Prelude.TimeOfDay -> CategoryCancellationInfo -> Environment.Flow (HighPrecMoney, Int)
cancelTBServiceCategory visitDate startTime CategoryCancellationInfo {..} = do
  resp <- mapM (cancelTBSPeopleCategory visitDate startTime) cancelledPeopleCategories
  let totalRefundAmount = sum $ fst <$> resp
      noOfCancelledSeats = sum $ snd <$> resp
      isEventCancelled = if cancelledCategory.bookedSeats - noOfCancelledSeats - (fromMaybe 0 cancelledCategory.cancelledSeats) == 0 then Just DTB.User else Nothing
  whenJust cancelledCategory.serviceCategoryId $ \id -> updateCancelledSeatsInSeatManagement (Kernel.Types.Id.Id id) noOfCancelledSeats visitDate

  QTBSC.updateCancellationDetailsById (Just $ noOfCancelledSeats + fromMaybe 0 cancelledCategory.cancelledSeats) (Just $ totalRefundAmount + fromMaybe 0 cancelledCategory.amountToRefund) isEventCancelled cancelledCategory.id

  pure (totalRefundAmount, noOfCancelledSeats)

cancelTBSPeopleCategory :: Data.Time.Calendar.Day -> Kernel.Prelude.TimeOfDay -> PeopleCategoryCancellationInfo -> Environment.Flow (HighPrecMoney, Int)
cancelTBSPeopleCategory visitDate startTime PeopleCategoryCancellationInfo {..} = do
  mbCancellationCharges <- case cancelledPeopleCategory.peopleCategoryId of
    Just peopleCategoryId -> do
      peopleCategory <- QPC.findServicePeopleCategoryById peopleCategoryId visitDate
      pure $ (.cancellationCharges) =<< peopleCategory
    Nothing -> pure Nothing
  cancellationCharge <- case mbCancellationCharges of
    Just cancellationCharges -> calculateCancellationChargesForPeopleCategory cancelledPeopleCategory cancelledQuantity cancellationCharges
    Nothing -> pure $ HighPrecMoney 0
  let amountToRefund = cancelledPeopleCategory.pricePerUnit.amount * fromIntegral cancelledQuantity - cancellationCharge

  QTBPC.updateTicketPeopleUnitsAndAmountById (Just $ cancelledQuantity + fromMaybe 0 cancelledPeopleCategory.numberOfUnitsCancelled) (Just $ amountToRefund + fromMaybe 0 cancelledPeopleCategory.amountToRefund) cancelledPeopleCategory.id
  pure (amountToRefund, cancelledQuantity)
  where
    calculateCancellationChargesForPeopleCategory :: DTB.TicketBookingPeopleCategory -> Int -> [Domain.Types.ServicePeopleCategory.CancellationCharge] -> Environment.Flow HighPrecMoney
    calculateCancellationChargesForPeopleCategory tBSPeopleCategory noOfTicketToCancel cancellationCharges' = do
      istCurrentTime <- getLocalCurrentTime 19800
      let visitDateTime = UTCTime visitDate (timeOfDayToTime startTime)
          remainingSeconds = max 0 (Kernel.Prelude.roundToIntegral $ diffUTCTime visitDateTime istCurrentTime)

      case filter (\cancellationCharge -> cancellationCharge.time <= remainingSeconds) $ sortBy (comparing (.time)) cancellationCharges' of
        [] -> pure $ HighPrecMoney 0
        cChange -> case (Kernel.Prelude.last cChange).cancelCharge of
          Domain.Types.ServicePeopleCategory.FlatFee fee ->
            let chargeAmount = min fee (tBSPeopleCategory.pricePerUnit.amount)
             in pure $ chargeAmount * fromIntegral noOfTicketToCancel
          Domain.Types.ServicePeopleCategory.Percentage per ->
            let chargeAmount = (HighPrecMoney (toRational per) * tBSPeopleCategory.pricePerUnit.amount * fromIntegral noOfTicketToCancel) / 100
             in pure chargeAmount

updateCancelledSeatsInSeatManagement :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Int -> Data.Time.Calendar.Day -> Environment.Flow ()
updateCancelledSeatsInSeatManagement categoryId noOfSeatsToCancel date = do
  mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate categoryId date
  whenJust mbSeatM $ \seatM -> QTSM.updateBookedSeats (max 0 $ seatM.booked - noOfSeatsToCancel) categoryId date

intializeRefundProcess :: Kernel.Types.Id.ShortId DTTB.TicketBooking -> Maybe $ Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> HighPrecMoney -> Kernel.Types.Id.Id Merchant.Merchant -> Kernel.Types.Id.Id MerchantOperatingCity.MerchantOperatingCity -> Maybe Text -> Maybe Version -> Environment.Flow ()
intializeRefundProcess ticketBookingShortId ticketPlaceId amountToRefund personMerchantId personMerchantOperatingCityId mRoutingId mSdkVersion = do
  refundId <- generateGUID
  let autoRefundReq =
        Payment.AutoRefundReq
          { orderId = ticketBookingShortId.getShortId,
            amount = amountToRefund,
            requestId = refundId
          }
      commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant personMerchantId
      createRefundCall = Payment.refundOrder personMerchantId personMerchantOperatingCityId ticketPlaceId Payment.Normal mRoutingId mSdkVersion
  void $ try @_ @SomeException $ DPayment.refundService (autoRefundReq, Kernel.Types.Id.Id {Kernel.Types.Id.getId = refundId}) commonMerchantId createRefundCall

tryUserTicketCancellationLock :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Environment.Flow () -> Environment.Flow ()
tryUserTicketCancellationLock businessHourId date serviceCategoryId executeFunction = do
  isMerchantLockAcquired <- isMerchantTicketCancellationLock businessHourId date serviceCategoryId
  when isMerchantLockAcquired $ throwError $ InvalidRequest "Merchant is cancelling the service"
  Redis.whenWithLockRedis lockKey 10 executeFunction
  where
    lockKey = mkUserTicketBookingCancellationKey businessHourId date serviceCategoryId

tryMerchantTicketCancellationLock :: (Redis.HedisFlow m r) => Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m Bool
tryMerchantTicketCancellationLock businessHourId date serviceCategoryId = do
  isUserLockAcquired <- isUserTicketCancellationLock businessHourId date serviceCategoryId
  when isUserLockAcquired $ throwError $ InvalidRequest "Some User Ticket cancellation is in progress, try again"
  Redis.tryLockRedis lockKey 10
  where
    lockKey = mkMerchantTicketBookingCancellationKey businessHourId date serviceCategoryId

isUserTicketCancellationLock :: (Redis.HedisFlow m r) => Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m Bool
isUserTicketCancellationLock businessHourId date serviceCategoryId = do
  (\(val :: Maybe Bool) -> isJust val) <$> Redis.safeGet (mkUserTicketBookingCancellationKey businessHourId date serviceCategoryId)

isMerchantTicketCancellationLock :: (Redis.HedisFlow m r) => Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m Bool
isMerchantTicketCancellationLock businessHourId date serviceCategoryId = do
  (\(val :: Maybe Bool) -> isJust val) <$> Redis.safeGet (mkMerchantTicketBookingCancellationKey businessHourId date serviceCategoryId)

mkMerchantTicketBookingCancellationKey :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Text
mkMerchantTicketBookingCancellationKey businessHourId date serviceCategoryId =
  "Merchant:ticket:booking:cancel:-" <> businessHourId.getId <> "-" <> show date <> "-" <> serviceCategoryId.getId

mkUserTicketBookingCancellationKey :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Text
mkUserTicketBookingCancellationKey businessHourId date serviceCategoryId =
  "User:ticket:booking:cancel:-" <> businessHourId.getId <> "-" <> show date <> "-" <> serviceCategoryId.getId

data BlockResult = BlockSuccess (Maybe Double) | BlockFailed Text deriving (Generic, Eq, Show, Read)

data LockBookingResult = LockBookingSuccess (Maybe RevertInfo) | LockBookingFailed deriving (Generic, Eq, Show, Read)

data RevertInfo = RevertInfo {revertKey :: Text, decrementBy :: Int} deriving (Generic, Eq, Show, Read)

tryChangeMaxCapacity :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Int -> Data.Time.Calendar.Day -> Environment.Flow ()
tryChangeMaxCapacity categoryId newMaxCapacity visitDate = do
  mbAllowedMaxCapacity <- Redis.get (mkTicketServiceAllowedMaxCapacityKey categoryId visitDate)
  let oldMaxCapacity = fromMaybe 0 mbAllowedMaxCapacity
  let change = fromIntegral $ newMaxCapacity - oldMaxCapacity
  if change > 0
    then do
      _ <- Redis.incrby (mkTicketServiceAllowedMaxCapacityKey categoryId visitDate) change
      QTSM.updateMaxCapacity (Just newMaxCapacity) categoryId visitDate
    else do
      seatM <- QTSM.findByTicketServiceCategoryIdAndDate categoryId visitDate >>= fromMaybeM (InternalError "Seat Management not found during negative change")
      mbBookedCount <- Redis.get (mkTicketServiceCategoryBookedCountKey categoryId visitDate)
      let bookedCount = fromMaybe seatM.booked mbBookedCount
      blockedCount <- getBlockCountInWindow categoryId visitDate
      if blockedCount + bookedCount >= newMaxCapacity
        then do
          throwError $ InvalidRequest "Cannot decrease max capacity as there might be more blocked + booked seats than the new max capacity"
        else do
          res <- Redis.decrby (mkTicketServiceAllowedMaxCapacityKey categoryId visitDate) (abs change)
          -- fallback if booked + block > newMaxCapacity
          mbNewBookedSeats <- Redis.get (mkTicketServiceCategoryBookedCountKey categoryId visitDate)
          let newBookedSeats = fromMaybe seatM.booked mbNewBookedSeats
          newBlockedCount <- getBlockCountInWindow categoryId visitDate
          if newBookedSeats + newBlockedCount > fromIntegral res
            then do
              _ <- Redis.incrby (mkTicketServiceAllowedMaxCapacityKey categoryId visitDate) (abs change)
              throwError $ InvalidRequest "Cannot decrease max capacity as there might be more blocked + booked seats than the new max capacity"
            else do
              QTSM.updateMaxCapacity (Just newMaxCapacity) categoryId visitDate

getBlockCountInWindow :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow Int
getBlockCountInWindow categoryId visitDate = do
  now <- getCurrentTimestamp
  blockedSet <- Redis.zRangeByScore (mkTicketServiceCategoryBlockedSeatKey categoryId visitDate) (now - 300000) now
  return $ accumulateCount blockedSet
  where
    accumulateCount arr = sum $ map extractCount arr

tryChangeBookedSeats :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Int -> Data.Time.Calendar.Day -> Environment.Flow ()
tryChangeBookedSeats categoryId newBookedSeats visitDate = do
  mbAllowedMaxCapacity <- Redis.get (mkTicketServiceAllowedMaxCapacityKey categoryId visitDate)
  seatM <- QTSM.findByTicketServiceCategoryIdAndDate categoryId visitDate >>= fromMaybeM (InternalError "Seat Management not found")
  let maxCapacity = fromMaybe (fromMaybe 0 seatM.maxCapacity) mbAllowedMaxCapacity
  if maxCapacity == 0
    then do
      QTSM.updateBookedSeats newBookedSeats categoryId visitDate
    else do
      bookedCount <- Redis.get (mkTicketServiceCategoryBookedCountKey categoryId visitDate)
      let change = fromIntegral $ newBookedSeats - fromMaybe seatM.booked bookedCount
      when (change < 0) $ throwError $ InvalidRequest "Invalid Operation, not allowed to decrease booked seats"
      blockedCount <- getBlockCountInWindow categoryId visitDate
      if newBookedSeats + blockedCount > maxCapacity
        then do
          throwError $ InvalidRequest "Cannot change max capacity as there might be more blocked + booked seats than the new max capacity"
        else do
          -- update and fallback
          res <- Redis.incrby (mkTicketServiceCategoryBookedCountKey categoryId visitDate) change
          if res > (fromIntegral maxCapacity)
            then do
              _ <- Redis.decrby (mkTicketServiceCategoryBookedCountKey categoryId visitDate) change
              throwError $ InvalidRequest "Cannot change max capacity as there might be more blocked + booked seats than the new max capacity"
            else do
              QTSM.safeUpdateBookedSeats newBookedSeats categoryId visitDate

tryBlockSeat :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory, Int) -> Environment.Flow BlockResult
tryBlockSeat personId visitDate (categoryId, numberOfUnits) = withActiveRequestCounter tryBlockServiceCategory
  where
    blockSeat :: Environment.Flow BlockResult
    blockSeat = do
      now <- getCurrentTimestamp
      Redis.zAdd (mkTicketServiceCategoryBlockedSeatKey categoryId visitDate) [(now, mkBlockMember personId numberOfUnits)]
      return $ BlockSuccess (Just $ now + 300000)
    tryBlockServiceCategory :: Int -> Environment.Flow BlockResult
    tryBlockServiceCategory requestCount = do
      mbAllowedMaxCapacity :: Maybe Int <- Redis.get (mkTicketServiceAllowedMaxCapacityKey categoryId visitDate)
      case mbAllowedMaxCapacity of
        Just allowedMaxCapacity -> do
          bookedCount :: Int <- Redis.get (mkTicketServiceCategoryBookedCountKey categoryId visitDate) >>= fromMaybeM (InternalError $ "Booked Count not found for categoryId " <> show categoryId <> "on date " <> show visitDate)
          if (bookedCount + numberOfUnits > allowedMaxCapacity)
            then return (BlockFailed $ (if (allowedMaxCapacity - bookedCount) > 0 then "Only " <> show (allowedMaxCapacity - bookedCount) else "No") <> " tickets left")
            else do
              blockedCountInWindow <- getBlockCountInWindow categoryId visitDate
              bookedCount' :: Int <- Redis.get (mkTicketServiceCategoryBookedCountKey categoryId visitDate) >>= fromMaybeM (InternalError $ "Booked Count not found for categoryId " <> show categoryId <> "on date " <> show visitDate)
              if (bookedCount' + blockedCountInWindow + requestCount) > allowedMaxCapacity
                then withConflictResolverCounter $ \resolverCounter -> do
                  blockedCountInWindow' <- getBlockCountInWindow categoryId visitDate
                  if resolverCounter > (allowedMaxCapacity - bookedCount - blockedCountInWindow')
                    then return $ BlockFailed "All Tickets Blocked for now, try again after some time"
                    else blockSeat
                else blockSeat
        Nothing -> pure $ BlockSuccess Nothing

    withActiveRequestCounter :: (Int -> Environment.Flow a) -> Environment.Flow a
    withActiveRequestCounter action = do
      requestCount <- Redis.incrby (mkTicketServiceCategoryActiveBlockRequestKey categoryId visitDate) (fromIntegral numberOfUnits)
      res <- action (fromIntegral requestCount)
      _ <- Redis.decrby (mkTicketServiceCategoryActiveBlockRequestKey categoryId visitDate) (fromIntegral numberOfUnits)
      return res

    withConflictResolverCounter :: (Int -> Environment.Flow a) -> Environment.Flow a
    withConflictResolverCounter action = do
      conflictResolverCount <- Redis.incrby (mkTicketServiceCategoryConflictResolverKey categoryId visitDate) (fromIntegral numberOfUnits)
      res <- action (fromIntegral conflictResolverCount)
      _ <- Redis.decrby (mkTicketServiceCategoryConflictResolverKey categoryId visitDate) (fromIntegral numberOfUnits)
      return res

extractCount :: BS.ByteString -> Int
extractCount bs = case BS8.split ':' bs of
  [_personId, countBS] -> case BS8.readInt countBS of
    Just (count, _) -> count
    Nothing -> 0
  _ -> 0

calculateTotalRequestedUnits :: TicketBookingReq -> [(Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory, Int)]
calculateTotalRequestedUnits req = Map.toList $ Map.fromListWith (+) [(c.categoryId, sum (map (.numberOfUnits) c.peopleCategories)) | s <- req.services, c <- s.categories]

blockSeats :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> [(Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory, Int)] -> Environment.Flow BlockResult
blockSeats personId visitDate requestedUnits = do
  res <- mapM (tryBlockSeat personId visitDate) requestedUnits
  let (failedBlocks, successfulBlocks) = partition (isBlockFailed . snd) (zip requestedUnits res)
  if null failedBlocks
    then do
      let blockExpiration = (fmap minimum . nonEmpty . catMaybes) [expiration | (_, BlockSuccess expiration) <- successfulBlocks]
      return $ BlockSuccess blockExpiration
    else do
      releaseBlock personId visitDate (map fst successfulBlocks)
      return $ BlockFailed (getErrorMessage $ snd <$> failedBlocks)
  where
    getErrorMessage failedBlocks = T.intercalate "\n" [msg | BlockFailed msg <- failedBlocks]

bookAndReleaseBlock :: Kernel.Types.Id.Id Domain.Types.Person.Person -> DTB.TicketBookingServiceCategory -> Environment.Flow LockBookingResult
bookAndReleaseBlock personId tbsc = withLogTag "Redis" $ do
  case (Kernel.Types.Id.Id <$> tbsc.serviceCategoryId, tbsc.visitDate) of
    (Just scId, Just date) -> do
      mbAllowedMaxCapacity :: Maybe Integer <- Redis.get (mkTicketServiceAllowedMaxCapacityKey scId date)
      case mbAllowedMaxCapacity of
        Just allowedMaxCapacity -> do
          res <- Redis.incrby (mkTicketServiceCategoryBookedCountKey scId date) (fromIntegral tbsc.bookedSeats)
          releaseBlock personId date [(scId, tbsc.bookedSeats)]
          if res > allowedMaxCapacity
            then do
              void $ Redis.decrby (mkTicketServiceCategoryBookedCountKey scId date) (fromIntegral tbsc.bookedSeats)
              return LockBookingFailed
            else do
              fork "Ticket Booking Garbage collection" $ garbageCollect scId date
              return $ LockBookingSuccess (Just $ RevertInfo (mkTicketServiceCategoryBookedCountKey scId date) tbsc.bookedSeats)
        Nothing -> return $ LockBookingSuccess Nothing
    _ -> return LockBookingFailed

tryLockBooking :: Kernel.Types.Id.Id Domain.Types.Person.Person -> [DTB.TicketBookingServiceCategory] -> Environment.Flow LockBookingResult
tryLockBooking personId tscs = do
  bookingResults <- mapM (bookAndReleaseBlock personId) tscs
  if any (== LockBookingFailed) bookingResults
    then do
      revertBookings (extractRevertInfo bookingResults)
      return LockBookingFailed
    else return (LockBookingSuccess Nothing)

isBlockFailed :: BlockResult -> Bool
isBlockFailed (BlockFailed _) = True
isBlockFailed _ = False

revertBookings :: [RevertInfo] -> Environment.Flow ()
revertBookings = mapM_ (\rInfo -> void $ Redis.decrby rInfo.revertKey (fromIntegral rInfo.decrementBy))

extractRevertInfo :: [LockBookingResult] -> [RevertInfo]
extractRevertInfo =
  catMaybes
    . map
      ( \case
          LockBookingFailed -> Nothing
          LockBookingSuccess rinfo -> rinfo
      )

garbageCollect :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow ()
garbageCollect scId date = do
  now <- getCurrentTimestamp
  void $ Redis.zRemRangeByScore (mkTicketServiceCategoryBlockedSeatKey scId date) 0 (now - 600000)

tryInstantBooking :: Kernel.Types.Id.Id Domain.Types.Person.Person -> [DTB.TicketBookingServiceCategory] -> Environment.Flow LockBookingResult
tryInstantBooking personId tscs = do
  case Kernel.Prelude.headMay tscs >>= (.visitDate) of
    Nothing -> return LockBookingFailed
    Just date -> do
      let requestedCount = map (\tsc -> tsc.serviceCategoryId >>= (\scId -> pure (Kernel.Types.Id.Id scId, tsc.bookedSeats))) tscs
      if any isNothing requestedCount
        then do
          return LockBookingFailed
        else do
          blockRes <- blockSeats personId date (catMaybes requestedCount)
          case blockRes of
            BlockFailed _ -> return LockBookingFailed
            BlockSuccess _ -> tryLockBooking personId tscs

postTicketDashboardRegister :: Domain.Types.Merchant.Merchant -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp
postTicketDashboardRegister merchant req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  personOpt <- QP.findByMobileNumberAndMerchantAndRole mobileNumberHash merchant.id [Domain.Types.Person.TICKET_DASHBOARD_USER]
  case personOpt of
    Just person ->
      return $
        API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp
          { success = True,
            message = Just "User was already registered on app side!",
            id = Just person.id.getId
          }
    Nothing -> do
      let key = makeMobileNumberOtpKey (req.mobileCountryCode <> req.mobileNumber)
      verificationOtp <- Redis.safeGet key >>= fromMaybeM (InvalidRequest "OTP expired")
      unless (verificationOtp == req.otp) $ throwError $ InvalidRequest "Invalid OTP"
      let authReq =
            Registration.AuthReq
              { mobileNumber = Just req.mobileNumber,
                mobileCountryCode = Just req.mobileCountryCode,
                identifierType = Just Domain.Types.Person.MOBILENUMBER,
                merchantId = merchant.shortId,
                deviceToken = Nothing,
                notificationToken = Nothing,
                whatsappNotificationEnroll = Nothing,
                firstName = Just req.firstName,
                middleName = Nothing,
                lastName = Just req.lastName,
                email = req.email,
                language = Nothing,
                gender = Nothing,
                otpChannel = Nothing,
                registrationLat = Nothing,
                registrationLon = Nothing,
                enableOtpLessRide = Nothing,
                allowBlockedUserLogin = Nothing
              }
      merchantOperatingCityId <-
        CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
          >>= fmap (.id)
            . fromMaybeM
              ( MerchantOperatingCityNotFound $
                  "merchantId: " <> merchant.id.getId <> " ,city: " <> show merchant.defaultCity
              )
      person <- Registration.buildPerson authReq Domain.Types.Person.MOBILENUMBER Nothing Nothing Nothing Nothing Nothing Nothing merchant merchant.defaultCity merchantOperatingCityId Nothing
      QP.create (person {Domain.Types.Person.role = Domain.Types.Person.TICKET_DASHBOARD_USER})
      return $
        API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp
          { success = True,
            message = Just "User registered successfully!",
            id = Just person.id.getId
          }

postTicketDashboardLoginAuth :: Domain.Types.Merchant.Merchant -> Tickets.TicketDashboardLoginReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketDashboardLoginAuth merchant req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- QP.findByMobileNumberAndMerchantAndRole mobileNumberHash merchant.id [Domain.Types.Person.TICKET_DASHBOARD_USER] >>= fromMaybeM (InvalidRequest "Mobile number not registered")
  checkSlidingWindowLimit (authHitsCountKey person)
  smsCfg <- asks (.smsCfg)
  merchantOperatingCityId <-
    CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
      >>= fmap (.id)
        . fromMaybeM
          ( MerchantOperatingCityNotFound $
              "merchantId: " <> merchant.id.getId <> " ,city: " <> show merchant.defaultCity
          )
  let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      countryCode = req.mobileCountryCode
      phoneNumber = countryCode <> req.mobileNumber
  if not person.blocked
    then do
      otp <- maybe generateOTPCode return useFakeOtpM
      whenNothing_ useFakeOtpM $ do
        let otpHash = smsCfg.credConfig.otpHash
        let otpCode = otp
        withLogTag ("personId_" <> person.id.getId) $ do
          buildSmsRes <-
            MessageBuilder.buildSendOTPMessage
              merchantOperatingCityId
              MessageBuilder.BuildSendOTPMessageReq
                { otp = otpCode,
                  hash = otpHash
                }
          Sms.sendSMS person.merchantId merchantOperatingCityId (buildSmsRes phoneNumber)
            >>= Sms.checkSmsResult
      let key = makeMobileNumberOtpKey phoneNumber
      Redis.setExp key otp 300
      return Success
    else throwError $ InvalidRequest "User is blocked"

makeMobileNumberOtpKey :: Text -> Text
makeMobileNumberOtpKey mobileNumber = "MobileNumberOtp:mobileNumber-" <> mobileNumber

authHitsCountKey :: Domain.Types.Person.Person -> Text
authHitsCountKey person = "BAP:Registration:auth" <> person.id.getId <> ":hitsCount"

verifyHitsCountKey :: Domain.Types.Person.Person -> Text
verifyHitsCountKey person = "BAP:Registration:verify:" <> person.id.getId <> ":hitsCount"

postTicketDashboardLoginVerify :: Domain.Types.Merchant.Merchant -> Tickets.TicketDashboardLoginReq -> Environment.Flow Tickets.TicketDashboardLoginResp
postTicketDashboardLoginVerify merchant req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- QP.findByMobileNumberAndMerchantAndRole mobileNumberHash merchant.id [Domain.Types.Person.TICKET_DASHBOARD_USER] >>= fromMaybeM (InvalidRequest "Invalid mobile number")
  checkSlidingWindowLimit (verifyHitsCountKey person)
  let key = makeMobileNumberOtpKey (req.mobileCountryCode <> req.mobileNumber)
  reqOtp <- req.otp & fromMaybeM (InvalidRequest "OTP required")
  otp <- Redis.safeGet key >>= fromMaybeM (InvalidRequest "OTP expired")
  unless (otp == reqOtp) $ throwError $ InvalidRequest "Invalid OTP"
  return $ Tickets.TicketDashboardLoginResp {authToken = Nothing}

getTicketDashboardAgreement :: Domain.Types.Merchant.Merchant -> Text -> Environment.Flow API.Types.Dashboard.AppManagement.Tickets.TicketDashboardAgreementTemplateResp
getTicketDashboardAgreement merchant templateName = do
  messageKey <- getMessageKey templateName
  merchantOperatingCityId <-
    CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
      >>= fmap (.id)
        . fromMaybeM
          ( MerchantOperatingCityNotFound $
              "merchantId: " <> merchant.id.getId <> " ,city: " <> show merchant.defaultCity
          )
  merchantMessage <- QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId messageKey Nothing >>= fromMaybeM (InvalidRequest "Template not found")
  return $ Tickets.TicketDashboardAgreementTemplateResp {template = merchantMessage.message}
  where
    getMessageKey = \case
      "TICKET_MERCHANT_AGREEMENT_TEMPLATE" -> pure $ DMM.TICKET_MERCHANT_AGREEMENT_TEMPLATE
      "TICKET_MERCHANT_SNLTR_TEMPLATE" -> pure $ DMM.TICKET_MERCHANT_SNLTR_TEMPLATE
      _ -> throwError $ InvalidRequest "Invalid template name"

sendverifyotpHitsCountKey :: Text -> Text
sendverifyotpHitsCountKey mobileNumber = "BAP:Registration:sendverifyotp:" <> mobileNumber

postTicketDashboardSendVerifyOtp :: Domain.Types.Merchant.Merchant -> Tickets.SendVerifyOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postTicketDashboardSendVerifyOtp merchant req = do
  let phoneNumber = req.mobileCountryCode <> req.mobileNumber
  let key = makeMobileNumberOtpKey phoneNumber
  checkSlidingWindowLimit $ sendverifyotpHitsCountKey phoneNumber
  mbOtp :: Maybe Text <- Redis.get key
  case mbOtp of
    Just _ -> throwError $ InvalidRequest "OTP already sent, please try again after 5 minutes"
    Nothing -> do
      smsCfg <- asks (.smsCfg)
      merchantOperatingCityId <-
        CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
          >>= fmap (.id)
            . fromMaybeM
              ( MerchantOperatingCityNotFound $
                  "merchantId: " <> merchant.id.getId <> " ,city: " <> show merchant.defaultCity
              )
      let useFakeOtpM = (show <$> useFakeSms smsCfg)
      otp <- maybe generateOTPCode return useFakeOtpM
      whenNothing_ useFakeOtpM $ do
        let otpHash = smsCfg.credConfig.otpHash
            otpCode = otp
        withLogTag ("mobileNumber_" <> phoneNumber) $ do
          buildSmsRes <-
            MessageBuilder.buildSendOTPMessage
              merchantOperatingCityId
              MessageBuilder.BuildSendOTPMessageReq
                { otp = otpCode,
                  hash = otpHash
                }
          Sms.sendSMS merchant.id merchantOperatingCityId (buildSmsRes phoneNumber) >>= Sms.checkSmsResult
      Redis.setExp key otp 300
  return Success

checkForBusinessHourOverrides ::
  Domain.Types.TicketService.TicketService ->
  [ ( [ ( Kernel.Types.Id.Id
            Domain.Types.ServiceCategory.ServiceCategory,
          Maybe [SharedLogic.TicketRule.Core.ActionType]
        )
      ],
      [ ( Kernel.Types.Id.Id
            Domain.Types.ServiceCategory.ServiceCategory,
          SharedLogic.TicketRule.Core.BusinessHourDef
        )
      ]
    )
  ] ->
  Environment.Flow (Maybe [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour])
checkForBusinessHourOverrides _service overrideDataList = do
  let (allScActionPairsNested, allScToOriginalBhtPairsNested) = (second concat . first (filter (isJust . snd) . concat) . unzip) overrideDataList
  let allScWithOverrideBHDefs ::
        [ ( Kernel.Types.Id.Id
              Domain.Types.ServiceCategory.ServiceCategory,
            [SharedLogic.TicketRule.Core.BusinessHourDef]
          )
        ] = map (\(scId, actions) -> (scId, maybe [] extractBusinessHrs actions)) allScActionPairsNested

  -- make a 2 maps one for orginial , and another for override Map ScId (Set BhDEf)
  let originalBHDefs = Map.fromListWith Set.union $ map (\(scId, bhs) -> (scId.getId, Set.singleton bhs)) allScToOriginalBhtPairsNested
  let overrideBHDefs = Map.fromListWith Set.union $ map (\(scId, bhs) -> (scId.getId, Set.fromList bhs)) allScWithOverrideBHDefs

  if not $ Map.null overrideBHDefs
    then do
      let finalBHDefs = Map.unionWith (\_ override -> override) originalBHDefs overrideBHDefs
      let hashDigest = sha1 finalBHDefs
      allHashedBHs <- QBH.findAllByHash (Just hashDigest)
      if null allHashedBHs then do createHashedBHs hashDigest finalBHDefs else pure Nothing
    else return Nothing
  where
    createHashedBHs :: Text -> Map.Map Text (Set.Set SharedLogic.TicketRule.Core.BusinessHourDef) -> Environment.Flow (Maybe [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour])
    createHashedBHs hashDigest localFinalBHDefs = do
      let allBhDefScIdPairs =
            concatMap
              ( \(scIdText, bhDefSet) ->
                  map (\bhDef -> (bhDef, scIdText)) (Set.toList bhDefSet)
              )
              (Map.toList localFinalBHDefs)
      let bhDefToScIdsMap = Map.fromListWith (++) $ map (\(bhDef, scIdText) -> (bhDef, [scIdText])) allBhDefScIdPairs
      newBhIds <- forM (Map.toList bhDefToScIdsMap) $ \(coreBhDef, serviceCatIdTexts) -> do
        newBhId <- generateGUID
        now <- getCurrentTime
        let newBusinessHour =
              Domain.Types.BusinessHour.BusinessHour
                { id = newBhId,
                  btype = convertCoreBHTtoDomainBHT coreBhDef.btype,
                  categoryId = map Kernel.Types.Id.Id serviceCatIdTexts,
                  hash = Just hashDigest,
                  expiryDate = Just $ addDays 5 (utctDay now),
                  name = Nothing,
                  placeId = Nothing,
                  bookingClosingTime = coreBhDef.bookingClosingTime,
                  merchantId = Nothing,
                  merchantOperatingCityId = Nothing,
                  createdAt = now,
                  updatedAt = now
                }
        QBH.create newBusinessHour
        pure newBhId
      pure $ Just newBhIds

    extractBusinessHrs :: [SharedLogic.TicketRule.Core.ActionType] -> [SharedLogic.TicketRule.Core.BusinessHourDef]
    extractBusinessHrs x =
      concat $
        mapMaybe
          ( \case
              SharedLogic.TicketRule.Core.OverrideBusinessHours bhs -> if bhs.serviceId == Kernel.Types.Id.getId _service.id then pure bhs.businessHours else Nothing
              _ -> Nothing
          )
          x

convertCoreBHTtoDomainBHT :: SharedLogic.TicketRule.Core.BusinessHourType -> Domain.Types.BusinessHour.BusinessHourType
convertCoreBHTtoDomainBHT (SharedLogic.TicketRule.Core.Slot t) = Domain.Types.BusinessHour.Slot t
convertCoreBHTtoDomainBHT (SharedLogic.TicketRule.Core.Duration st ed) = Domain.Types.BusinessHour.Duration st ed

convertDomainBHTtoCoreBHT :: Domain.Types.BusinessHour.BusinessHourType -> SharedLogic.TicketRule.Core.BusinessHourType
convertDomainBHTtoCoreBHT (Domain.Types.BusinessHour.Slot t) = SharedLogic.TicketRule.Core.Slot t
convertDomainBHTtoCoreBHT (Domain.Types.BusinessHour.Duration st ed) = SharedLogic.TicketRule.Core.Duration st ed

sha1 :: ToJSON a => a -> Text
sha1 = T.pack . show . Hash.hashWith Hash.SHA1 . BS.toStrict . encode
