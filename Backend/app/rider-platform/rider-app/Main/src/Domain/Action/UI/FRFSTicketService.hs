{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Status as ACL
import Control.Monad.Extra hiding (fromMaybeM)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTrip as DFRFSTrip
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (id, map)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface as Payment
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

getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations _ vehicleType_ = do
  stations <- B.runInReplica $ QS.getTicketPlacesByVehicleType vehicleType_
  return $
    map
      ( \Station.Station {..} ->
          FRFSTicketService.FRFSStationAPI
            { color = Nothing,
              stationType = Nothing,
              ..
            }
      )
      stations

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (mbPersonId, merchantId) vehicleType_ FRFSSearchAPIReq {..} = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
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
    gatewayUrl <- parseBaseUrl "https://gateway.beckn.org" & fromMaybeM (InvalidRequest "Invalid gateway url") -- TODO: get Gateway url from Merchant
    bknSearchReq <- ACL.buildSearchReq searchReq fromStation toStation
    void $ CallBPP.search gatewayUrl bknSearchReq
  return $ FRFSSearchAPIRes searchReqId

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote (mbPersonId, _) searchId_ = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  search <- QFRFSSearch.findById searchId_ >>= fromMaybeM (InvalidRequest "Invalid search id")
  unless (personId == search.riderId) $ throwError AccessDenied
  (quotes :: [DFRFSQuote.FRFSQuote]) <- B.runInReplica $ QFRFSQuote.findAllBySearchId searchId_

  mapM
    ( \quote -> do
        -- trips <- B.runInReplica $ QFRFSTrip.findAllByQuoteId quote.id
        -- let _ = sortBy (\tripA tripB -> compare tripA.stopSequence tripB.stopSequence) trips
        (stations :: [FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
        -- let stations =
        --       map
        --         ( \DFRFSTrip.FRFSTrip {..} ->
        --             FRFSTicketService.FRFSStationAPI
        --               { address = Nothing,
        --                 code = stationCode,
        --                 color = Nothing,
        --                 lat = Nothing,
        --                 lon = Nothing,
        --                 name = stationName,
        --                 stationType = Just stationType,
        --                 ..
        --               }
        --         )
        --         trips'
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
  dConfirmRes <- confirm
  -- handle (errHandler dConfirmRes.booking) $
  --   void $ withShortRetry $ CallBPP.init dConfirmRes.bppSubscriberUrl becknInitReq
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")

  -- fork "FRFS Init Req" $ do
  when (isNothing dConfirmRes.bppOrderId) $ do
    providerUrl <- dConfirmRes.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bknInitReq <- ACL.buildInitReq dConfirmRes
    void $ CallBPP.init providerUrl bknInitReq
  return $ makeBookingStatusAPI dConfirmRes stations
  where
    -- errHandler booking exc
    --   | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelFRFSTicketBooking booking
    --   | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelFRFSTicketBooking booking
    --   | otherwise = throwM exc

    confirm = do
      personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
      _ <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
      unless (personId == quote.riderId) $ throwError AccessDenied
      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ InvalidRequest "Quote expired"
      maybeM (buildBooking quote) (\booking -> return booking) (QFRFSTicketBooking.findByQuoteId quoteId)

    buildBooking DFRFSQuote.FRFSQuote {..} = do
      uuid <- generateGUID
      now <- getCurrentTime
      return $
        DFRFSTicketBooking.FRFSTicketBooking
          { id = uuid,
            bppOrderId = Nothing,
            quoteId = id,
            status = DFRFSTicketBooking.NEW,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId_,
            ..
          }

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
          ..
        }

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, _) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")

  callBPPStatus booking

  unless (personId == booking.riderId) $ throwError AccessDenied
  stations <- decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  (paymentBooking :: Maybe DFRFSTicketBookingPayment.FRFSTicketBookingPayment) <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId
  payment <- case paymentBooking of
    Just paymentBooking' -> do
      paymentOrder' <- QPaymentOrder.findById paymentBooking'.paymentOrderId
      paymentTransaction' <- QPaymentTransaction.findNewTransactionByOrderId paymentBooking'.paymentOrderId
      case (paymentTransaction', paymentOrder') of
        (Just paymentTransaction, Just paymentOrder) -> do
          person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
          personEmail <- mapM decrypt person.email
          personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
          let createOrderReq =
                Payment.CreateOrderReq
                  { orderId = paymentOrder.id.getId,
                    orderShortId = paymentOrder.shortId.getShortId,
                    amount = paymentOrder.amount,
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
          sdk_payload' <- DPayment.buildSDKPayload createOrderReq paymentOrder
          case sdk_payload' of
            Just sdk_payload -> do
              return $
                Just
                  FRFSTicketService.FRFSBookingPaymentAPI
                    { status = makeTicketBookingPaymentAPIStatus paymentTransaction.status,
                      paymentOrder =
                        Payment.CreateOrderResp
                          { status = paymentOrder.status,
                            id = paymentOrder.paymentServiceOrderId,
                            order_id = paymentOrder.shortId.getShortId,
                            payment_links = Just paymentOrder.paymentLinks,
                            sdk_payload
                          }
                    }
            Nothing -> return Nothing
        (_, _) -> return Nothing
    Nothing -> return Nothing
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
        ..
      }

callBPPStatus :: DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow ()
callBPPStatus booking = do
  fork "FRFS Init Req" $ do
    providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
    bknStatusReq <- ACL.buildStatusReq booking
    void $ CallBPP.status providerUrl bknStatusReq

-- payment = Nothing,

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, _) = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId personId
  mapM
    ( \booking -> do
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
              payment = Nothing,
              ..
            }
    )
    bookings

makeTicketBookingPaymentAPIStatus :: Payment.TransactionStatus -> FRFSTicketService.FRFSBookingPaymentStatusAPI
makeTicketBookingPaymentAPIStatus NEW = FRFSTicketService.PENDING
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
