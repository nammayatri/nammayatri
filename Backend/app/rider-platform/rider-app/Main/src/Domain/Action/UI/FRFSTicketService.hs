{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTrip as DFRFSTrip
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant as QMerc
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
postFrfsQuoteConfirm = error "Logic yet to be decided"

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, _) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  unless (personId == booking.riderId) $ throwError AccessDenied
  stations <- decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  -- (paymentBooking :: Maybe DFRFSTicketBookingPayment.FRFSTicketBookingPayment) <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId
  -- payment <- case paymentBooking of
  --   Just paymentBooking' -> do
  --     -- paymentOrder <- B.runInReplica $ QPaymentOrder.findById paymentBooking'.paymentOrderId
  --     paymentTransaction <- QPaymentTransaction.findNewTransactionByOrderId paymentBooking'.paymentOrderId
  --     case paymentTransaction of
  --       Just paymentTransaction' ->
  --         return $
  --           Just
  --             FRFSTicketService.FRFSBookingPaymentAPI
  --               { status = makeTicketBookingPaymentAPIStatus paymentTransaction'.status
  --               }
  --       Nothing -> return Nothing
  --   Nothing -> return Nothing
  -- let paymentOrderId' :: Maybe (Id DPaymentOrder.PaymentOrder) = (.paymentOrderId) <$> paymentBooking
  -- paymentTransaction <- traverse (\(orderId :: (Id DPaymentOrder.PaymentOrder)) ->  B.runInReplica $ QPaymentTransaction.findNewTransactionByOrderId orderId) Nothing
  -- -- paymentTransaction <- B.runInReplica $ QPaymentTransaction.findByOrderId (QPaymentOrder.id <$> paymentOrder)
  -- let payment = case paymentTransaction of
  --       Just paymentTransaction' ->
  --         Just
  --           FRFSTicketService.FRFSBookingPaymentAPI
  --             { status = makeTicketBookingPaymentAPIStatus paymentTransaction'.status
  --             }
  --       Nothing -> Nothing

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

-- findByShortId

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
        -- quote <- B.runInReplica $ QFRFSQuote.findById booking.quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
        -- trips <- B.runInReplica $ QFRFSTrip.findAllByQuoteId quote.id
        -- let trips' = sortBy (\tripA tripB -> compare tripA.stopSequence tripB.stopSequence) trips
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
