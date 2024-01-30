{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Status as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import Control.Monad.Extra hiding (fromMaybeM)
import Data.OpenApi (ToSchema)
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTrip as DFRFSTrip
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (id, map)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Servant hiding (throwError)
import qualified SharedLogic.CallFRFSBPP as CallBPP
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTrip as QFRFSTrip
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Station as QS
import qualified Storage.Queries.Station as QStation
import Tools.Auth
import Tools.Error
import qualified Tools.Payment as Payment

getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations _ vehicleType_ = do
  stations <- B.runInReplica $ QS.getTicketPlacesByVehicleType vehicleType_
  return $
    map
      ( \Station.Station {..} ->
          FRFSTicketService.FRFSStationAPI
            { color = Nothing,
              stationType = Nothing,
              sequenceNum = Nothing,
              ..
            }
      )
      stations

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (mbPersonId, merchantId) vehicleType_ FRFSSearchAPIReq {..} = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdAndDomain (Just merchant.id) (show Spec.FRFS) >>= fromMaybeM (InternalError "Beckn Config not found")
  fromStation <- QStation.findByStationCode fromStationCode >>= fromMaybeM (InvalidRequest "Invalid from station id")
  toStation <- QStation.findByStationCode toStationCode >>= fromMaybeM (InvalidRequest "Invalid to station id")

  searchReqId <- generateGUID
  now <- getCurrentTime

  let searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchReqId,
            vehicleType = vehicleType_,
            merchantId = Just merchantId,
            merchantOperatingCityId = Nothing,
            createdAt = now,
            updatedAt = now,
            fromStationId = fromStation.id,
            toStationId = toStation.id,
            riderId = personId,
            ..
          }
  QFRFSSearch.create searchReq
  fork "FRFS SearchReq" $ do
    bknSearchReq <- ACL.buildSearchReq searchReq bapConfig fromStation toStation
    logDebug $ "FRFS SearchReq " <> (encodeToText bknSearchReq)
    void $ CallBPP.search bapConfig.gatewayUrl bknSearchReq
  return $ FRFSSearchAPIRes searchReqId

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote (mbPersonId, _) searchId_ = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  search <- QFRFSSearch.findById searchId_ >>= fromMaybeM (InvalidRequest "Invalid search id")
  unless (personId == search.riderId) $ throwError AccessDenied
  (quotes :: [DFRFSQuote.FRFSQuote]) <- B.runInReplica $ QFRFSQuote.findAllBySearchId searchId_

  mapM
    ( \quote -> do
        (stations :: [FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
        return $
          FRFSTicketService.FRFSQuoteAPIRes
            { quoteId = quote.id,
              _type = quote._type,
              price = quote.price,
              quantity = quote.quantity,
              validTill = quote.validTill,
              vehicleType = quote.vehicleType,
              ..
            }
    )
    quotes

postFrfsQuoteConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm (mbPersonId, merchantId_) quoteId = do
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  (rider, dConfirmRes) <- confirm
  -- handle (errHandler dConfirmRes.booking) $
  --   void $ withShortRetry $ CallBPP.init dConfirmRes.bppSubscriberUrl becknInitReq
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  now <- getCurrentTime

  when (dConfirmRes.status == DFRFSTicketBooking.NEW && dConfirmRes.validTill > now) $ do
    providerUrl <- dConfirmRes.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bapConfig <- QBC.findByMerchantIdAndDomain (Just merchant.id) (show Spec.FRFS) >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) dConfirmRes bapConfig Utils.BppData {bppId = dConfirmRes.bppSubscriberId, bppUri = dConfirmRes.bppSubscriberUrl}
    logDebug $ "FRFS SearchReq " <> (encodeToText bknInitReq)
    void $ CallBPP.init providerUrl bknInitReq
  return $ makeBookingStatusAPI dConfirmRes stations
  where
    -- errHandler booking exc
    --   | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelFRFSTicketBooking booking
    --   | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelFRFSTicketBooking booking
    --   | otherwise = throwM exc

    confirm = do
      personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
      unless (personId == quote.riderId) $ throwError AccessDenied
      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ InvalidRequest "Quote expired"
      maybeM (buildAndCreateBooking rider quote) (\booking -> return (rider, booking)) (QFRFSTicketBooking.findByQuoteId quoteId)

    buildAndCreateBooking rider quote@DFRFSQuote.FRFSQuote {..} = do
      uuid <- generateGUID
      now <- getCurrentTime
      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                bppOrderId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = Just merchantId_,
                price = HighPrecMoney $ (quote.price.getHighPrecMoney) * (toRational quote.quantity),
                ..
              }
      QFRFSTicketBooking.create booking
      return (rider, booking)

    makeBookingStatusAPI booking stations =
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          _type = booking._type,
          price = booking.price,
          quantity = booking.quantity,
          validTill = booking.validTill,
          vehicleType = booking.vehicleType,
          status = booking.status,
          payment = Nothing,
          tickets = [],
          createdAt = booking.createdAt,
          ..
        }

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdAndDomain (Just merchant.id) (show Spec.FRFS) >>= fromMaybeM (InternalError "Beckn Config not found")
  booking' <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  unless (personId == booking'.riderId) $ throwError AccessDenied
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  when (booking'.status /= DFRFSTicketBooking.CONFIRMED && booking'.status /= DFRFSTicketBooking.FAILED && booking'.validTill < now) $
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  case booking.status of
    DFRFSTicketBooking.NEW -> buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.FAILED -> buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.CONFIRMING -> do
      buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      callBPPStatus booking bapConfig
      buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.APPROVED -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id orderStatusCall
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      if (paymentBookingStatus == FRFSTicketService.FAILURE)
        then do
          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else do
          txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
          let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.PAYMENT_PENDING Nothing
          paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId
          let paymentObj =
                Just $
                  FRFSTicketService.FRFSBookingPaymentAPI
                    { status = paymentStatus_,
                      paymentOrder = Just paymentOrder_
                    }
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentObj
    DFRFSTicketBooking.PAYMENT_PENDING -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id orderStatusCall
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      if (paymentBookingStatus == FRFSTicketService.FAILURE)
        then do
          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS)
            then do
              let updatedTTL = addUTCTime (60 :: NominalDiffTime) now -- 1 min from time of payment success
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.SUCCESS booking.id
              void $ QFRFSTicketBooking.updateValidTillAndStatusById DFRFSTicketBooking.CONFIRMING updatedTTL booking.id
              transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
              txnId <- getSuccessTransactionId transactions
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL)
              fork "FRFS Confirm Req" $ do
                providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
                let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                mRiderNumber <- mapM decrypt person.mobileNumber
                bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) updatedBooking bapConfig txnId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl}
                logDebug $ "FRFS SearchReq " <> (encodeToText bknConfirmReq)
                void $ CallBPP.confirm providerUrl bknConfirmReq
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentSuccess
            else do
              paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId
              txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
              let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                  paymentObj =
                    Just
                      FRFSTicketService.FRFSBookingPaymentAPI
                        { status = paymentStatus_,
                          paymentOrder = Just paymentOrder_
                        }
              buildFRFSTicketBookingStatusAPIRes booking paymentObj
  where
    paymentSuccess =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.SUCCESS,
            paymentOrder = Nothing
          }

    paymentFailed =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.FAILURE,
            paymentOrder = Nothing
          }

    buildCreateOrderResp paymentOrder person commonPersonId = do
      personEmail <- mapM decrypt person.email
      personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
      let createOrderReq =
            Payment.CreateOrderReq
              { orderId = paymentOrder.id.getId,
                orderShortId = paymentOrder.shortId.getShortId,
                amount = paymentOrder.amount,
                customerId = person.id.getId,
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
      DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall >>= fromMaybeM (PaymentOrderDoesNotExist paymentOrder.id.getId)

    createOrderCall = Payment.createOrder merchantId_
    orderStatusCall = Payment.orderStatus merchantId_
    commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId_

    makeUpdatedBooking DFRFSTicketBooking.FRFSTicketBooking {..} updatedStatus mTTL =
      let validTill' = mTTL & fromMaybe validTill
       in DFRFSTicketBooking.FRFSTicketBooking {status = updatedStatus, validTill = validTill', ..}
    getSuccessTransactionId transactions = do
      let successTransactions = filter (\transaction -> transaction.status == Payment.CHARGED) transactions
      case successTransactions of
        [] -> throwError $ InvalidRequest "No successful transaction found"
        [transaction] -> return transaction.id
        _ -> throwError $ InvalidRequest "Multiple successful transactions found"

callBPPStatus :: DFRFSTicketBooking.FRFSTicketBooking -> BecknConfig -> Environment.Flow ()
callBPPStatus booking bapConfig = do
  fork "FRFS Status Req" $ do
    providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bknStatusReq <- ACL.buildStatusReq booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl}
    logDebug $ "FRFS SearchReq " <> (encodeToText bknStatusReq)
    void $ CallBPP.status providerUrl bknStatusReq

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, _) = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId personId
  mapM (\booking -> buildFRFSTicketBookingStatusAPIRes booking Nothing) bookings

buildFRFSTicketBookingStatusAPIRes :: DFRFSTicketBooking.FRFSTicketBooking -> Maybe FRFSTicketService.FRFSBookingPaymentAPI -> Environment.Flow FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking payment = do
  stations <- decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {..} ->
              FRFSTicketService.FRFSTicketAPI {..}
          )
          tickets'
  return $
    FRFSTicketService.FRFSTicketBookingStatusAPIRes
      { bookingId = booking.id,
        _type = booking._type,
        price = booking.price,
        quantity = booking.quantity,
        validTill = booking.validTill,
        vehicleType = booking.vehicleType,
        status = booking.status,
        createdAt = booking.createdAt,
        ..
      }

makeTicketBookingPaymentAPIStatus :: Payment.TransactionStatus -> FRFSTicketService.FRFSBookingPaymentStatusAPI
makeTicketBookingPaymentAPIStatus Payment.NEW = FRFSTicketService.NEW
makeTicketBookingPaymentAPIStatus PENDING_VBV = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus CHARGED = FRFSTicketService.SUCCESS
makeTicketBookingPaymentAPIStatus AUTHENTICATION_FAILED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZATION_FAILED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus JUSPAY_DECLINED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZING = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus COD_INITIATED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus STARTED = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus AUTO_REFUNDED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus CLIENT_AUTH_TOKEN_EXPIRED = FRFSTicketService.FAILURE

cancelFRFSTicketBooking :: DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow ()
cancelFRFSTicketBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
