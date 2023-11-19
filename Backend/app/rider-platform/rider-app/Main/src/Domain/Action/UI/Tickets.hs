{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Tickets where

import Data.List (groupBy)
import Data.Time (Day, UTCTime (UTCTime), timeOfDayToTime, utctDay)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Tickets as DTB
import qualified Domain.Types.Tickets.TicketBooking as DTTB
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Tickets.TicketBooking as QTB
import qualified Storage.Queries.Tickets.TicketBookingService as QTBS
import qualified Storage.Queries.Tickets.TicketPlace as QTP
import qualified Storage.Queries.Tickets.TicketService as QTS
import Tools.Error
import qualified Tools.Payment as Payment

data TicketServiceReq = TicketServiceReq
  { serviceId :: Id DTB.TicketService,
    attendeeType :: Text,
    numberOfUnits :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingReq = TicketBookingReq
  { visitDate :: Day,
    services :: [TicketServiceReq]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingAPIEntity = TicketBookingAPIEntity
  { ticketShortId :: Text,
    ticketPlaceId :: Text,
    ticketPlaceName :: Text,
    personId :: Text,
    amount :: HighPrecMoney,
    visitDate :: Day,
    status :: DTTB.BookingStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingDetails = TicketBookingDetails
  { ticketShortId :: Text,
    ticketPlaceId :: Text,
    ticketPlaceName :: Text,
    personId :: Text,
    amount :: HighPrecMoney,
    visitDate :: Day,
    status :: DTTB.BookingStatus,
    services :: [TicketBookingServiceDetails]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketBookingServiceDetails = TicketBookingServiceDetails
  { ticketServiceShortId :: Text,
    ticketServiceName :: Text,
    amount :: HighPrecMoney,
    status :: DTB.ServiceStatus,
    verificationCount :: Int,
    expiryDate :: Maybe UTCTime,
    prices :: [DTB.TicketBookingServicePriceBreakup]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketVerificationStatus
  = BookingSuccess
  | BookingExpired
  | BookingFuture
  | BookingAlreadyVerified
  | DifferentService
  | PaymentPending
  | InvalidBooking
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data TicketServiceVerificationResp = TicketServiceVerificationResp
  { ticketServiceName :: Maybe Text,
    visitDate :: Maybe Day,
    validTill :: Maybe UTCTime,
    ticketServiceShortId :: Maybe Text,
    message :: Text,
    status :: TicketVerificationStatus,
    amount :: Maybe HighPrecMoney,
    verificationCount :: Maybe Int,
    units :: Maybe [DTB.TicketBookingServicePriceBreakup]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets TicketServiceVerificationResp where
  hideSecrets = identity

getTicketPlaces :: (Id DP.Person, Id Merchant.Merchant) -> Flow [DTB.TicketPlace]
getTicketPlaces (_, merchantId) = do
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId
  QTP.getTicketPlaces merchantOpCity.id

getTicketServices :: (Id DP.Person, Id Merchant.Merchant) -> Id DTB.TicketPlace -> Flow [DTB.TicketService]
getTicketServices (_, _) = QTS.getTicketServicesByPlaceId

bookTicket :: (Id DP.Person, Id Merchant.Merchant) -> Id DTB.TicketPlace -> TicketBookingReq -> Flow CreateOrderResp
bookTicket (personId, merchantId) placeId req = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId

  ticketBookingId <- generateGUID
  let ticketServiceReqs = groupBy ((==) `on` serviceId) req.services
  ticketBookingServices <- mapM (createTicketBookingService merchantOpCity.id ticketBookingId) ticketServiceReqs

  let amount = sum (map (.amount) ticketBookingServices)
  ticketBooking <- createTicketBooking merchantOpCity.id ticketBookingId amount

  QTBS.createMany ticketBookingServices
  QTB.create ticketBooking

  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let createOrderReq =
        CreateOrderReq
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
  let commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder merchantId -- api call
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return createOrderRes
    Nothing -> do
      throwError $ InternalError "Failed to create order"
  where
    createTicketBooking merchantOperatingCityId ticketBookingId amount = do
      shortId <- generateShortId
      now <- getCurrentTime
      return $
        DTTB.TicketBooking
          { id = ticketBookingId,
            shortId,
            merchantOperatingCityId,
            ticketPlaceId = placeId,
            personId = cast personId,
            amount,
            visitDate = req.visitDate,
            status = DTTB.Pending,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingService merchantOperatingCityId ticketBookingId ticketServiceReq = do
      let ticketServiceId = (.serviceId) (head ticketServiceReq) -- using head it can't be empty
      ticketServiceConfig <- QTS.findById ticketServiceId >>= fromMaybeM (TicketServiceNotFound ticketServiceId.getId)
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      prices <- mapM (createTicketBookingServicePriceBreakup id ticketServiceConfig) ticketServiceReq
      let amount = sum (map (\price -> price.pricePerUnit * fromIntegral price.numberOfUnits) prices)
      return $
        DTB.TicketBookingService
          { id,
            shortId,
            ticketBookingId,
            ticketServiceId,
            amount,
            status = DTB.Pending,
            verificationCount = 0,
            expiryDate = (\time -> UTCTime req.visitDate (timeOfDayToTime time)) <$> ticketServiceConfig.validityTimings,
            merchantOperatingCityId,
            prices,
            createdAt = now,
            updatedAt = now
          }

    createTicketBookingServicePriceBreakup :: Id DTB.TicketBookingService -> DTB.TicketService -> TicketServiceReq -> Flow DTB.TicketBookingServicePriceBreakup
    createTicketBookingServicePriceBreakup ticketBookingServiceId ticketServiceConfig ticketServiceReqEntity = do
      let mTicketServiceConfigPrice = find (\t -> t.attendeeType == ticketServiceReqEntity.attendeeType) ticketServiceConfig.prices
      ticketServiceConfigPrice <- mTicketServiceConfigPrice & fromMaybeM (InvalidRequest "Invalid attendeeType")
      return $
        DTB.TicketBookingServicePriceBreakup
          { ticketBookingServiceId = ticketBookingServiceId,
            attendeeType = (.attendeeType) ticketServiceReqEntity,
            numberOfUnits = (.numberOfUnits) ticketServiceReqEntity,
            pricePerUnit = ticketServiceConfigPrice.pricePerUnit
          }

getAllBookings :: (Id DP.Person, Id Merchant.Merchant) -> DTTB.BookingStatus -> Maybe Int -> Maybe Int -> Flow [TicketBookingAPIEntity]
getAllBookings (personId_, merchantId) status_ mbLimit mbOffset = do
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId
  ticketBookings <- QTB.getAllBookingsByPersonId personId_ merchantOpCity.id status_ mbLimit mbOffset
  convertToApiEntity `mapM` ticketBookings
  where
    convertToApiEntity :: DTTB.TicketBooking -> Flow TicketBookingAPIEntity
    convertToApiEntity DTTB.TicketBooking {..} = do
      ticketPlace <- QTP.findById ticketPlaceId >>= fromMaybeM (TicketPlaceNotFound ticketPlaceId.getId)
      return $
        TicketBookingAPIEntity
          { ticketShortId = getShortId shortId,
            ticketPlaceId = getId ticketPlaceId,
            personId = personId.getId,
            ticketPlaceName = ticketPlace.name,
            ..
          }

getBookingDetail :: (Id DP.Person, Id Merchant.Merchant) -> ShortId DTTB.TicketBooking -> Flow TicketBookingDetails
getBookingDetail _ shortId_ = do
  ticketBooking <- QTB.findByShortId shortId_ >>= fromMaybeM (TicketBookingNotFound shortId_.getShortId)
  ticketBookingServices <- QTBS.findAllByBookingId ticketBooking.id
  services <- mapM mkTicketBookingServiceDetails ticketBookingServices
  mkTicketBookingDetails ticketBooking services
  where
    mkTicketBookingServiceDetails :: DTB.TicketBookingService -> Flow TicketBookingServiceDetails
    mkTicketBookingServiceDetails DTB.TicketBookingService {..} = do
      ticketServiceConfig <- QTS.findById ticketServiceId >>= fromMaybeM (TicketServiceNotFound ticketServiceId.getId)
      return $
        TicketBookingServiceDetails
          { ticketServiceShortId = shortId.getShortId,
            ticketServiceName = ticketServiceConfig.service,
            ..
          }

    mkTicketBookingDetails :: DTTB.TicketBooking -> [TicketBookingServiceDetails] -> Flow TicketBookingDetails
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

verifyBookingDetails :: Id DTB.TicketService -> ShortId DTB.TicketBookingService -> Flow TicketServiceVerificationResp
verifyBookingDetails = processBookingService
  where
    processBookingService :: Id DTB.TicketService -> ShortId DTB.TicketBookingService -> Flow TicketServiceVerificationResp
    processBookingService personServiceId serviceShortId = do
      mBookingService <- QTBS.findByShortId serviceShortId
      case mBookingService of
        Just bookingService -> do
          (mTicketServiceConfig, mBooking) <- liftM2 (,) (QTS.findById bookingService.ticketServiceId) (QTB.findById bookingService.ticketBookingId)
          case (mTicketServiceConfig, mBooking) of
            (Just ticketServiceConfig, Just booking) -> processValidBooking bookingService ticketServiceConfig booking personServiceId
            _ -> return $ createVerificationResp InvalidBooking Nothing Nothing Nothing
        Nothing -> return $ createVerificationResp InvalidBooking Nothing Nothing Nothing

    processValidBooking :: DTB.TicketBookingService -> DTB.TicketService -> DTTB.TicketBooking -> Id DTB.TicketService -> Flow TicketServiceVerificationResp
    processValidBooking bookingService ticketServiceConfig booking personServiceId
      | bookingService.ticketServiceId /= personServiceId = return $ createVerificationResp DifferentService Nothing (Just ticketServiceConfig) Nothing
      | otherwise = case bookingService.status of
        DTB.Pending -> return $ createVerificationResp PaymentPending (Just bookingService) (Just ticketServiceConfig) (Just booking)
        DTB.Failed -> return $ createVerificationResp InvalidBooking (Just bookingService) (Just ticketServiceConfig) (Just booking)
        DTB.Verified -> handleConfirmedBooking bookingService ticketServiceConfig booking
        DTB.Confirmed -> handleConfirmedBooking bookingService ticketServiceConfig booking

    handleConfirmedBooking :: DTB.TicketBookingService -> DTB.TicketService -> DTTB.TicketBooking -> Flow TicketServiceVerificationResp
    handleConfirmedBooking bookingService ticketServiceConfig booking = do
      now <- getCurrentTime
      case bookingService.expiryDate of
        Just expiry ->
          if expiry < now
            then return $ createVerificationResp BookingExpired (Just bookingService) (Just ticketServiceConfig) (Just booking)
            else handleConfirmedNonExpiredBooking bookingService ticketServiceConfig booking
        Nothing -> handleConfirmedNonExpiredBooking bookingService ticketServiceConfig booking

    handleConfirmedNonExpiredBooking :: DTB.TicketBookingService -> DTB.TicketService -> DTTB.TicketBooking -> Flow TicketServiceVerificationResp
    handleConfirmedNonExpiredBooking bookingService ticketServiceConfig booking = do
      now <- getCurrentTime
      if booking.visitDate > utctDay now
        then do return $ createVerificationResp BookingFuture (Just bookingService) (Just ticketServiceConfig) (Just booking)
        else do handleVerifiedBooking bookingService ticketServiceConfig booking

    handleVerifiedBooking :: DTB.TicketBookingService -> DTB.TicketService -> DTTB.TicketBooking -> Flow TicketServiceVerificationResp
    handleVerifiedBooking bookingService ticketServiceConfig booking
      | bookingService.verificationCount >= ticketServiceConfig.maxVerification =
        return $ createVerificationResp BookingAlreadyVerified (Just bookingService) (Just ticketServiceConfig) (Just booking)
      | otherwise = do
        now <- getCurrentTime
        QTBS.updateVerification bookingService.id (bookingService.verificationCount + 1) now
        return $ createVerificationResp BookingSuccess (Just bookingService) (Just ticketServiceConfig) (Just booking)

    createVerificationResp :: TicketVerificationStatus -> Maybe DTB.TicketBookingService -> Maybe DTB.TicketService -> Maybe DTTB.TicketBooking -> TicketServiceVerificationResp
    createVerificationResp status mBookingService mTicketServiceConfig mBooking = do
      TicketServiceVerificationResp
        { ticketServiceName = mTicketServiceConfig <&> (.service),
          visitDate = mBooking <&> (.visitDate),
          validTill = mBookingService >>= (.expiryDate) >>= (Just . addUTCTime (secondsToNominalDiffTime 19800)), -- 19800 for +5:30 timezone
          ticketServiceShortId = mBookingService <&> (.shortId) <&> (.getShortId),
          message = verificationMsg status,
          status,
          amount = mBookingService <&> (.amount),
          verificationCount = mBookingService <&> (.verificationCount),
          units = mBookingService <&> (.prices)
        }

    verificationMsg :: TicketVerificationStatus -> Text
    verificationMsg BookingSuccess = "Validated successfully!"
    verificationMsg BookingExpired = "Booking Expired!"
    verificationMsg BookingFuture = "Booking for Later Date!"
    verificationMsg BookingAlreadyVerified = "Already Validated!"
    verificationMsg DifferentService = "Different Service!"
    verificationMsg PaymentPending = "Payment Pending!"
    verificationMsg InvalidBooking = "Not a valid QR"

getBookingStatus :: (Id DP.Person, Id Merchant.Merchant) -> ShortId DTTB.TicketBooking -> Flow DTTB.BookingStatus
getBookingStatus (personId, merchantId) (ShortId shortId) = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  order <- QOrder.findByShortId (ShortId shortId) >>= fromMaybeM (PaymentOrderNotFound shortId)
  ticketBooking' <- QTB.findByShortId (ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId)
  if order.status == Payment.CHARGED -- Consider CHARGED status as terminal status
    then return ticketBooking'.status
    else do
      paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
      case paymentStatus of
        DPayment.PaymentStatus {..} -> do
          when (status == Payment.CHARGED) $ do
            QTB.updateStatusByShortId (ShortId shortId) DTTB.Booked
            QTBS.updateAllStatusByBookingId ticketBooking'.id DTB.Confirmed
          when (status `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]) $ do
            QTB.updateStatusByShortId (ShortId shortId) DTTB.Failed
            QTBS.updateAllStatusByBookingId ticketBooking'.id DTB.Failed
        _ -> return ()
      ticketBooking <- QTB.findByShortId (ShortId shortId) >>= fromMaybeM (TicketBookingNotFound shortId) -- fetch again for updated status
      return ticketBooking.status
