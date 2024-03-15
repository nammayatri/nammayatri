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
import qualified Domain.Action.Beckn.FRFS.Common as Common
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
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
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (all, and, id, map)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (MerchantError (MerchantOperatingCityNotFound))
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
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

getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Context.City -> Station.FRFSVehicleType -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations (_personId, mId) mbCity vehicleType_ = do
  case mbCity of
    Nothing -> return []
    Just city -> do
      merchantOpCity <- CQMOC.findByMerchantIdAndCity mId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)
      stations <- B.runInReplica $ QS.getTicketPlacesByMerchantOperatingCityIdAndVehicleType merchantOpCity.id vehicleType_
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
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) METRO >>= fromMaybeM (InternalError "Beckn Config not found")
  fromStation <- QStation.findByStationCode fromStationCode >>= fromMaybeM (InvalidRequest "Invalid from station id")
  toStation <- QStation.findByStationCode toStationCode >>= fromMaybeM (InvalidRequest "Invalid to station id")
  merchantOperatingCity <- CQMOC.findById fromStation.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show fromStation.merchantOperatingCityId)

  searchReqId <- generateGUID
  now <- getCurrentTime

  let searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchReqId,
            vehicleType = vehicleType_,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just fromStation.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            fromStationId = fromStation.id,
            toStationId = toStation.id,
            riderId = personId,
            ..
          }
  QFRFSSearch.create searchReq
  fork "FRFS SearchReq" $ do
    bknSearchReq <- ACL.buildSearchReq searchReq bapConfig fromStation toStation merchantOperatingCity.city
    logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
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
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  now <- getCurrentTime

  when (dConfirmRes.status == DFRFSTicketBooking.NEW && dConfirmRes.validTill > now) $ do
    providerUrl <- dConfirmRes.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) METRO >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    -- Add default TTL of 30 seconds or the value provided in the config
    let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
    void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
    let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
    bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) dConfirmRes' bapConfig Utils.BppData {bppId = dConfirmRes.bppSubscriberId, bppUri = dConfirmRes.bppSubscriberUrl} merchantOperatingCity.city
    logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
    void $ CallBPP.init providerUrl bknInitReq
  return $ makeBookingStatusAPI dConfirmRes stations merchantOperatingCity.city
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
                estimatedPrice = HighPrecMoney $ (quote.price.getHighPrecMoney) * (toRational quote.quantity),
                finalPrice = Nothing,
                paymentTxnId = Nothing,
                bppBankAccountNumber = Nothing,
                bppBankCode = Nothing,
                cancellationCharges = Nothing,
                refundAmount = Nothing,
                isBookingCancellable = Nothing,
                ..
              }
      QFRFSTicketBooking.create booking
      return (rider, booking)

    makeBookingStatusAPI booking stations city =
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          city,
          updatedAt = booking.updatedAt,
          createdAt = booking.createdAt,
          _type = booking._type,
          price = booking.price,
          quantity = booking.quantity,
          validTill = booking.validTill,
          vehicleType = booking.vehicleType,
          status = booking.status,
          payment = Nothing,
          tickets = [],
          ..
        }

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

webhookHandlerFRFSTicket :: Kernel.Types.Id.ShortId DPaymentOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Environment.Flow ()
webhookHandlerFRFSTicket paymentOrderId merchantId = do
  logDebug $ "frfs ticket order bap webhookc call" <> paymentOrderId.getShortId
  order <- QPaymentOrder.findByShortId paymentOrderId >>= fromMaybeM (PaymentOrderNotFound paymentOrderId.getShortId)
  bookingByOrderId <- QFRFSTicketBookingPayment.findByPaymentOrderId order.id >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
  booking' <- B.runInReplica $ QFRFSTicketBooking.findById bookingByOrderId.frfsTicketBookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  void $ getFrfsBookingStatus (Just booking'.riderId, merchantId) booking'.id

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) METRO >>= fromMaybeM (InternalError "Beckn Config not found")
  booking' <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  unless (personId == booking'.riderId) $ throwError AccessDenied
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  when (booking'.status /= DFRFSTicketBooking.CONFIRMED && booking'.status /= DFRFSTicketBooking.FAILED && booking'.status /= DFRFSTicketBooking.CANCELLED && booking'.validTill < now) $
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking booking
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  case booking.status of
    DFRFSTicketBooking.NEW -> buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.FAILED -> buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.CONFIRMING -> do
      buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      callBPPStatus booking bapConfig merchantOperatingCity.city
      buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.APPROVED -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id)
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
          paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id
          let paymentObj =
                Just $
                  FRFSTicketService.FRFSBookingPaymentAPI
                    { status = paymentStatus_,
                      paymentOrder = paymentOrder_
                    }
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentObj
    DFRFSTicketBooking.PAYMENT_PENDING -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id)
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
              -- Add default TTL of 1 min or the value provided in the config
              let updatedTTL = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
              transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
              txnId <- getSuccessTransactionId transactions
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.SUCCESS booking.id
              void $ QFRFSTicketBooking.updateStatusValidTillAndPaymentTxnById DFRFSTicketBooking.CONFIRMING updatedTTL (Just txnId.getId) booking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL)
              fork "FRFS Confirm Req" $ do
                providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
                let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                mRiderNumber <- mapM decrypt person.mobileNumber
                bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) updatedBooking bapConfig txnId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
                logDebug $ "FRFS ConfirmReq " <> encodeToText bknConfirmReq
                void $ CallBPP.confirm providerUrl bknConfirmReq
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentSuccess
            else do
              paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id
              txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
              let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                  paymentObj =
                    Just
                      FRFSTicketService.FRFSBookingPaymentAPI
                        { status = paymentStatus_,
                          paymentOrder = paymentOrder_
                        }
              buildFRFSTicketBookingStatusAPIRes booking paymentObj
    DFRFSTicketBooking.CANCELLED -> buildFRFSTicketBookingStatusAPIRes booking Nothing
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

    buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCityId = do
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
                mandateStartDate = Nothing,
                optionsGetUpiDeepLinks = Nothing,
                metadataExpiryInMins = Nothing,
                metadataGatewayReferenceId = Nothing --- assigned in shared kernel
              }
      DPayment.createOrderService commonMerchantId commonPersonId createOrderReq (createOrderCall merchantOperatingCityId)

    createOrderCall merchantOperatingCityId = Payment.createOrder merchantId_ merchantOperatingCityId Nothing Payment.FRFSBooking
    orderStatusCall merchantOperatingCityId = Payment.orderStatus merchantId_ merchantOperatingCityId Nothing Payment.FRFSBooking
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

callBPPStatus :: DFRFSTicketBooking.FRFSTicketBooking -> BecknConfig -> Context.City -> Environment.Flow ()
callBPPStatus booking bapConfig city = do
  fork "FRFS Status Req" $ do
    providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bknStatusReq <- ACL.buildStatusReq booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} city
    logDebug $ "FRFS StatusReq " <> encodeToText bknStatusReq
    void $ CallBPP.status providerUrl bknStatusReq

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, _) = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId personId
  mapM (\booking -> buildFRFSTicketBookingStatusAPIRes booking Nothing) bookings

buildFRFSTicketBookingStatusAPIRes :: DFRFSTicketBooking.FRFSTicketBooking -> Maybe FRFSTicketService.FRFSBookingPaymentAPI -> Environment.Flow FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking payment = do
  stations <- decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking booking
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
        city = merchantOperatingCity.city,
        updatedAt = booking.updatedAt,
        createdAt = booking.createdAt,
        _type = booking._type,
        price = booking.price,
        quantity = booking.quantity,
        validTill = booking.validTill,
        vehicleType = booking.vehicleType,
        status = booking.status,
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

postFrfsBookingCanCancel :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow APISuccess.APISuccess
postFrfsBookingCanCancel (_, merchantId) bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) METRO >>= fromMaybeM (InternalError "Beckn Config not found")
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid ticketBookingId")

  unless (ticketBooking.status == DFRFSTicketBooking.CONFIRMED) $ throwError (InvalidRequest "Cancellation during incorrect status")
  -- tickets <- QFRFSTicket.findAllByTicketBookingId ticketBooking.id
  -- unless (all (\ticket -> ticket.status == DFRFSTicket.ACTIVE) tickets) $ throwError (InvalidRequest "Cancellation during incorrect status")
  DACFOC.callBPPCancel ticketBooking bapConfig Spec.SOFT_CANCEL
  return APISuccess.Success

getFrfsBookingCanCancelStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSCanCancelStatus
getFrfsBookingCanCancelStatus _ bookingId = do
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid ticketBookingId")
  return $
    FRFSCanCancelStatus
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = getAbsoluteValue ticketBooking.refundAmount,
        isCancellable = ticketBooking.isBookingCancellable
      }

postFrfsBookingCancel :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow APISuccess.APISuccess
postFrfsBookingCancel (_, merchantId) bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) METRO >>= fromMaybeM (InternalError "Beckn Config not found")
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  DACFOC.callBPPCancel ticketBooking bapConfig Spec.CONFIRM_CANCEL
  return APISuccess.Success

getFrfsBookingCancelStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow FRFSTicketService.FRFSCancelStatus
getFrfsBookingCancelStatus _ bookingId = do
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  pure
    FRFSTicketService.FRFSCancelStatus
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = if (ticketBooking.status == DFRFSTicketBooking.CANCELLED) then getAbsoluteValue ticketBooking.refundAmount else Nothing
      }

getAbsoluteValue :: Maybe HighPrecMoney -> Maybe HighPrecMoney
getAbsoluteValue mbRefundAmount = case mbRefundAmount of
  Nothing -> Nothing
  Just rfValue -> do
    let HighPrecMoney value = rfValue
    Just (HighPrecMoney $ abs value)
