{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Action.UI.MultimodalConfirm
  ( postMultimodalInitiate,
    postMultimodalInitiateSimpl,
    postMultimodalConfirm,
    getMultimodalBookingInfo,
    postMultimodalOrderSwitchTaxi,
    getMultimodalBookingPaymentStatus,
    postMultimodalSwitch,
    postMultimodalRiderLocation,
    postMultimodalJourneyCancel,
    postMultimodalExtendLeg,
    postMultimodalJourneyLegSkip,
    postMultimodalJourneyLegAddSkippedLeg,
    getMultimodalJourneyStatus,
    postMultimodalExtendLegGetfare,
    postMultimodalJourneyFeedback,
    getMultimodalFeedback,
    getMultimodalUserPreferences,
    postMultimodalUserPreferences,
    postMultimodalTransitOptionsLite,
    postMultimodalOrderSwitchFRFSTier,
    getPublicTransportData,
    getPublicTransportVehicleData,
    getMultimodalOrderGetLegTierOptions,
    postMultimodalPaymentUpdateOrder,
    postMultimodalOrderSublegSetStatus,
    postMultimodalOrderSublegSetTrackingStatus,
    postMultimodalTicketVerify,
    postMultimodalComplete,
    postMultimodalOrderSoftCancel,
    getMultimodalOrderCancelStatus,
    postMultimodalOrderCancel,
    getMultimodalOrderSimilarJourneyLegs,
    postMultimodalOrderSwitchJourneyLeg,
    postMultimodalOrderChangeStops,
    postMultimodalRouteServiceability,
    postMultimodalRouteAvailability,
    postMultimodalSwitchRoute,
    postMultimodalOrderSublegSetOnboardedVehicleDetails,
    postMultimodalSetRouteName,
    postMultimodalUpdateBusLocation,
    postStoreTowerInfo,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified API.UI.CancelSearch as CancelSearch
import qualified API.UI.Rating as Rating
import BecknV2.FRFS.Enums
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Concurrent (modifyMVar)
import Control.Monad.Extra (mapMaybeM)
import qualified Data.HashMap.Strict as HashMap
import Data.Int ()
import Data.List (nub, nubBy, partition)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), defaultTimeLocale, diffTimeToPicoseconds, formatTime, parseTimeM)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.BBPS as BBPS
import qualified Domain.Action.UI.Dispatcher as Dispatcher
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Action.UI.ParkingBooking as ParkingBooking
import qualified Domain.Action.UI.Pass as Pass
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DEst
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyFeedback as JFB
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.RouteDetails as RD
import qualified Domain.Types.RouteStopMapping as DRSM
import Domain.Types.RouteStopTimeTable (SourceType (..))
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Station as DStation
import Domain.Utils (castTravelModeToVehicleCategory, mapConcurrently)
import Environment
import EulerHS.Prelude hiding (all, any, catMaybes, concatMap, elem, find, forM_, groupBy, id, length, map, mapM_, null, readMaybe, sum, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import qualified ExternalBPP.ExternalAPI.CallAPI as DirectExternalBPP
import qualified ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Google.MapsClient.Types (LatLngV2 (..), LocationV2 (..), WayPointV2 (..))
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude hiding (foldl')
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.TH
import qualified Lib.JourneyLeg.Common.FRFS as JLCF
import qualified Lib.JourneyLeg.Types as JL
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyModule.Base
import qualified Lib.JourneyModule.Base as JM
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Lib.JourneyModule.Utils as JLU
import qualified Lib.JourneyModule.Utils as JMU
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.Cancel as SharedCancel
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.Payment as SPayment
import qualified SharedLogic.Utils as SLUtils
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DeviceVehicleMapping as QDeviceVehicleMapping
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import Storage.Queries.JourneyFeedback as SQJFB
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.JourneyLegExtra as QJourneyLegExtra
import qualified Storage.Queries.JourneyLegMapping as QJourneyLegMapping
import Storage.Queries.MultimodalPreferences as QMP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderConfig as QRiderConfig
import qualified Storage.Queries.RouteDetails as QRouteDetails
import Storage.Queries.SearchRequest as QSearchRequest
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Tools.Error
import qualified Tools.Error as StationError
import qualified Tools.Metrics as Metrics
import Tools.MultiModal as MM
import qualified Tools.MultiModal as TMultiModal
import qualified Tools.Payment as Payment
import qualified Tools.Wallet as TWallet

-- Custom JSON instance for PublicTransportData with omitNothingFields
-- Type of these fields are Defined in Backend/app/rider-platform/rider-app/Main/src-read-only/API/Types/UI/MultimodalConfirm.hs
$(deriveJSONOmitNothing ''API.Types.UI.MultimodalConfirm.TransportRoute)
$(deriveJSONOmitNothing ''API.Types.UI.MultimodalConfirm.TransportStation)
$(deriveJSONOmitNothing ''API.Types.UI.MultimodalConfirm.TransportRouteStopMapping)
$(deriveJSONOmitNothing ''API.Types.UI.MultimodalConfirm.PublicTransportData)

runAction :: Id Domain.Types.Journey.Journey -> Environment.Flow ApiTypes.JourneyInfoResp -> Environment.Flow ApiTypes.JourneyInfoResp
runAction journeyId action = do
  if journeyId.getId == ""
    then action
    else do
      Redis.withWaitAndLockRedis lockKey 10 100 $
        action
  where
    lockKey = "infoLock-" <> journeyId.getId

postMultimodalInitiate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
postMultimodalInitiate (_personId, _merchantId) journeyId filterServiceAndJrnyType = do
  runAction journeyId $ do
    journeyLegs <- QJourneyLeg.getJourneyLegs journeyId
    journey <- JM.getJourney journeyId
    let blacklistedServiceTiers = if filterServiceAndJrnyType == Just False then [] else [Spec.AC_EMU_FIRST_CLASS]
    let blacklistedFareQuoteTypes = if filterServiceAndJrnyType == Just False then [] else [DFRFSQuote.ReturnJourney]
    JMU.measureLatency (addAllLegs journey (Just journeyLegs) journeyLegs blacklistedServiceTiers blacklistedFareQuoteTypes) "addAllLegs"
    JM.updateJourneyStatus journey Domain.Types.Journey.INITIATED
    legs <- JMU.measureLatency (JM.getAllLegsInfo journey.riderId journey.id) "JM.getAllLegsInfo"
    JMU.measureLatency (generateJourneyInfoResponse journey legs) "generateJourneyInfoResponse"

postMultimodalInitiateSimpl ::
  [DJourneyLeg.JourneyLeg] ->
  Domain.Types.Journey.Journey ->
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  Environment.Flow ApiTypes.JourneyInfoResp
postMultimodalInitiateSimpl journeyLegs journey blacklistedServiceTiers blacklistedFareQuoteTypes = do
  runAction journey.id $ do
    JMU.measureLatency (addAllLegs journey (Just journeyLegs) journeyLegs blacklistedServiceTiers blacklistedFareQuoteTypes) "addAllLegs"
    JM.updateJourneyStatus journey Domain.Types.Journey.INITIATED
    legs <- JMU.measureLatency (JM.getAllLegsInfo journey.riderId journey.id) "JM.getAllLegsInfo"
    JMU.measureLatency (generateJourneyInfoResponse journey legs) "generateJourneyInfoResponse"

postMultimodalConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    API.Types.UI.MultimodalConfirm.JourneyConfirmReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyConfirmResp
  )
postMultimodalConfirm (_mbPersonId, _merchantId) journeyId forcedBookLegOrder mbIsMockPayment journeyConfirmReq = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journey.id
  let confirmElements = journeyConfirmReq.journeyConfirmReqElements
      isMockPayment = fromMaybe False mbIsMockPayment
  void $ JM.startJourney journey.riderId confirmElements forcedBookLegOrder journey journeyConfirmReq.enableOffer (Just isMockPayment)
  -- If all FRFS legs are skipped, update journey status to INPROGRESS. Otherwise, update journey status to CONFIRMED and it would be marked as INPROGRESS on Payment Success in `markJourneyPaymentSuccess`.
  -- Note: INPROGRESS journey status indicates that the tracking has started.
  if isAllFRFSLegSkipped legs confirmElements
    then do
      JM.updateJourneyStatus journey Domain.Types.Journey.INPROGRESS
      fork "Analytics - Journey Skip Without Booking Update" $ QJourney.updateHasStartedTrackingWithoutBooking (Just True) journeyId
    else JM.updateJourneyStatus journey Domain.Types.Journey.CONFIRMED
  fork "Caching recent location" $ JLU.createRecentLocationForMultimodal journey
  updatedJourney <- JM.getJourney journeyId
  paymentGateWayId <- Payment.fetchGatewayReferenceId journey.merchantId journey.merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking
  sdkPayload <-
    case updatedJourney.paymentOrderShortId of
      Just paymentOrderShortId -> do
        QOrder.findByShortId paymentOrderShortId
          >>= \case
            Just paymentOrder -> do
              person <- QP.findById journey.riderId >>= fromMaybeM (InvalidRequest "Person not found")
              let isSingleMode = fromMaybe False journey.isSingleMode
              buildCreateOrderResp paymentOrder journey.riderId journey.merchantOperatingCityId person Payment.FRFSMultiModalBooking isSingleMode
            Nothing -> return Nothing
      Nothing -> return Nothing
  pure ApiTypes.JourneyConfirmResp {ApiTypes.orderSdkPayload = sdkPayload, ApiTypes.gatewayReferenceId = paymentGateWayId, result = "Success"}
  where
    isAllFRFSLegSkipped legs journeyConfirmReqElements =
      all
        ( \leg -> maybe True (.skipBooking) (find (\r -> r.journeyLegOrder == leg.sequenceNumber && leg.mode `elem` [DTrip.Bus, DTrip.Subway, DTrip.Metro]) journeyConfirmReqElements)
        )
        legs

getMultimodalBookingInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
getMultimodalBookingInfo (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey legs

getMultimodalBookingPaymentStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyBookingPaymentStatus
  )
getMultimodalBookingPaymentStatus (mbPersonId, merchantId) journeyId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  legs <- QJourneyLeg.getJourneyLegs journeyId
  allJourneyFrfsBookings <- mapMaybeM (QFRFSTicketBooking.findBySearchId . Id) (mapMaybe (.legSearchId) legs)
  let isSingleMode = case allJourneyFrfsBookings of
        [_] -> True
        _ -> False
  bookingsPaymentOrders <-
    mapM
      ( \booking -> do
          QFRFSTicketBookingPayment.findTicketBookingPayment booking
            >>= \case
              Just paymentBooking -> do
                QPaymentOrder.findById paymentBooking.paymentOrderId
                  >>= \case
                    Just paymentOrder -> do
                      let paymentServiceType = fromMaybe Payment.FRFSMultiModalBooking paymentOrder.paymentServiceType
                          merchantOperatingCityId = fromMaybe booking.merchantOperatingCityId (cast <$> paymentOrder.merchantOperatingCityId)
                          orderStatusCall = Payment.orderStatus merchantId merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
                          fulfillmentHandler = mkFulfillmentHandler paymentServiceType paymentOrder.id
                      void $ SPayment.orderStatusHandler fulfillmentHandler paymentServiceType paymentOrder orderStatusCall
                      createOrderResp <- buildCreateOrderResp paymentOrder personId merchantOperatingCityId person paymentServiceType isSingleMode
                      return (createOrderResp, Just paymentBooking.status)
                    Nothing -> return (Nothing, Nothing)
              Nothing -> return (Nothing, Nothing)
      )
      allJourneyFrfsBookings
  paymentGateWayId <- Payment.fetchGatewayReferenceId merchantId person.merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking
  let anyFirstBookingPaymentOrder = listToMaybe bookingsPaymentOrders
      paymentOrder =
        case anyFirstBookingPaymentOrder of
          Just (Just paymentOrder', Just paymentBookingStatus) ->
            Just $
              ApiTypes.PaymentOrder
                { sdkPayload = Just paymentOrder',
                  status = mkDomainPaymentStatusToAPIStatus paymentBookingStatus
                }
          _ -> Nothing
      allPaymentOrders = all (isJust . fst) bookingsPaymentOrders
  if allPaymentOrders
    then do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder,
            gatewayReferenceId = paymentGateWayId
          }
    else do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder = Nothing,
            gatewayReferenceId = paymentGateWayId
          }
  where
    mkDomainPaymentStatusToAPIStatus :: DFRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> FRFSTicketServiceAPI.FRFSBookingPaymentStatusAPI
    mkDomainPaymentStatusToAPIStatus = \case
      DFRFSTicketBookingPayment.PENDING -> FRFSTicketServiceAPI.PENDING
      DFRFSTicketBookingPayment.SUCCESS -> FRFSTicketServiceAPI.SUCCESS
      DFRFSTicketBookingPayment.FAILED -> FRFSTicketServiceAPI.FAILURE
      DFRFSTicketBookingPayment.REFUND_PENDING -> FRFSTicketServiceAPI.REFUND_PENDING
      DFRFSTicketBookingPayment.REFUNDED -> FRFSTicketServiceAPI.REFUNDED
      DFRFSTicketBookingPayment.REFUND_FAILED -> FRFSTicketServiceAPI.REFUND_FAILED
      DFRFSTicketBookingPayment.REFUND_INITIATED -> FRFSTicketServiceAPI.REFUND_INITIATED

    mkFulfillmentHandler paymentServiceType orderId paymentStatusResp = case paymentServiceType of
      DOrder.FRFSBooking -> FRFSTicketService.frfsOrderStatusHandler merchantId paymentStatusResp JMU.switchFRFSQuoteTierUtil
      DOrder.FRFSBusBooking -> FRFSTicketService.frfsOrderStatusHandler merchantId paymentStatusResp JMU.switchFRFSQuoteTierUtil
      DOrder.FRFSMultiModalBooking -> FRFSTicketService.frfsOrderStatusHandler merchantId paymentStatusResp JMU.switchFRFSQuoteTierUtil
      DOrder.FRFSPassPurchase -> do
        status <- DPayment.getTransactionStatus paymentStatusResp
        Pass.passOrderStatusHandler orderId merchantId status
      DOrder.ParkingBooking -> do
        status <- DPayment.getTransactionStatus paymentStatusResp
        ParkingBooking.parkingBookingOrderStatusHandler orderId merchantId status
      DOrder.BBPS -> do
        paymentFulfillStatus <- BBPS.bbpsOrderStatusHandler merchantId paymentStatusResp
        pure (paymentFulfillStatus, Nothing, Nothing)
      _ -> pure (DPayment.FulfillmentPending, Nothing, Nothing)

buildCreateOrderResp :: DOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Id DMOC.MerchantOperatingCity -> Domain.Types.Person.Person -> Payment.PaymentServiceType -> Bool -> Environment.Flow (Maybe Payment.CreateOrderResp)
buildCreateOrderResp paymentOrder personId merchantOperatingCityId person paymentServiceType isSingleMode = do
  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  isSplitEnabled_ <- Payment.getIsSplitEnabled (cast paymentOrder.merchantId) merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking
  isPercentageSplitEnabled <- Payment.getIsPercentageSplit (cast paymentOrder.merchantId) merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking
  splitSettlementDetails <- Payment.mkSplitSettlementDetails isSplitEnabled_ paymentOrder.amount [] isPercentageSplitEnabled isSingleMode
  staticCustomerId <- SLUtils.getStaticCustomerId person personPhone
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = paymentOrder.id.getId,
            orderShortId = paymentOrder.shortId.getShortId,
            amount = paymentOrder.amount,
            customerId = staticCustomerId,
            customerEmail = fromMaybe "growth@nammayatri.in" personEmail,
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
            metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
            splitSettlementDetails = splitSettlementDetails,
            basket = Nothing
          }
  mbPaymentOrderValidTill <- Payment.getPaymentOrderValidity (cast paymentOrder.merchantId) merchantOperatingCityId Nothing paymentServiceType
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let createOrderCall = Payment.createOrder (cast paymentOrder.merchantId) merchantOperatingCityId Nothing paymentServiceType (Just personId.getId) person.clientSdkVersion paymentOrder.isMockPayment
      createWalletCall = TWallet.createWallet person.merchantId person.merchantOperatingCityId
  DPayment.createOrderService paymentOrder.merchantId (Just $ cast merchantOperatingCityId) (cast personId) mbPaymentOrderValidTill Nothing paymentServiceType isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) (fromMaybe False paymentOrder.isMockPayment) Nothing

-- TODO :: To be deprecated @Kavyashree
postMultimodalPaymentUpdateOrder ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.UpdatePaymentOrderReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.UpdatePaymentOrderResp
  )
postMultimodalPaymentUpdateOrder (_, _) _ _req = throwError $ InvalidRequest "Not implemented"

postMultimodalOrderSwitchTaxi ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.SwitchTaxiReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSwitchTaxi (_, _) journeyId legOrder req = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  unless (journeyLeg.mode == DTrip.Taxi) $
    throwError (JourneyLegCannotBeCancelled (show journeyLeg.sequenceNumber))

  whenJust journeyLeg.legSearchId $ \legSearchId -> do
    searchReq <- QSearchRequest.findById (Id legSearchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legSearchId)
    mbEstimate <- maybe (pure Nothing) (QEstimate.findById . Id) journeyLeg.legPricingId
    whenJust mbEstimate $ \estimate -> do
      when (estimate.status `elem` [DEst.COMPLETED, DEst.CANCELLED, DEst.GOT_DRIVER_QUOTE, DEst.DRIVER_QUOTE_CANCELLED]) $
        throwError $ InvalidRequest "Can't switch vehicle if driver has already being assigned"
      QJourneyLeg.updateLegPricingIdByLegSearchId (Just req.estimateId.getId) journeyLeg.legSearchId
      when (estimate.status == DEst.DRIVER_QUOTE_REQUESTED) $ do
        cancelPrevSearch searchReq legSearchId estimate.id
        JMTypes.confirm (mkTaxiLegConfirmReq searchReq req.estimateId)

  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
  where
    mkTaxiLegConfirmReq :: DSR.SearchRequest -> Id DEstimate.Estimate -> TaxiLegRequest
    mkTaxiLegConfirmReq searchReq estimateId = do
      TaxiLegRequestConfirm $
        TaxiLegRequestConfirmData
          { bookLater = False,
            forcedBooked = True,
            searchId = searchReq.id.getId,
            estimateId = Just estimateId,
            startTime = searchReq.startTime,
            personId = searchReq.riderId,
            merchantId = searchReq.merchantId
          }
    cancelPrevSearch searchReq legSearchId estimateId = do
      cancelResponse <- CancelSearch.cancelSearch' (searchReq.riderId, searchReq.merchantId) estimateId
      case cancelResponse of
        SharedCancel.Success -> return ()
        SharedCancel.BookingAlreadyCreated -> throwError (InternalError $ "Cannot cancel search as booking is already created for searchId: " <> show legSearchId)
        SharedCancel.FailedToCancel -> throwError (InvalidRequest $ "Failed to cancel search for searchId: " <> show legSearchId)

postMultimodalOrderSwitchFRFSTier ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.SwitchFRFSTierReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSwitchFRFSTier (_mbPersonId, _merchantId) journeyId legOrder req = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  JMU.switchFRFSQuoteTierUtil journeyLeg req.quoteId
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs

postMultimodalSwitch ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  ApiTypes.SwitchLegReq ->
  Environment.Flow ApiTypes.JourneyInfoResp
postMultimodalSwitch userInfo@(mbPersonId, _) journeyId filterServiceAndJrnyType req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Invalid person id")
  JM.switchLeg journeyId personId req filterServiceAndJrnyType
  getMultimodalBookingInfo userInfo journeyId

postMultimodalRiderLocation ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.RiderLocationReq ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalRiderLocation _ journeyId req = do
  (journeyStatus, legs) <- getMultimodalJourneyStatusAndLegs journeyId
  let concatLegs =
        concatMap
          ( \leg -> case leg of
              JMTypes.Transit transitLegs -> transitLegs
              JMTypes.Single singleLeg -> [singleLeg]
          )
          legs
  let busLeg = find (\leg -> leg.mode == DTrip.Bus && leg.status == JL.Ongoing) concatLegs
  addPoint journeyId req ((.fleetNo) =<< busLeg)
  return journeyStatus

postMultimodalJourneyCancel ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyCancel (_, _) _ = throwError $ InvalidRequest "Not implemented"

postMultimodalExtendLeg ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.ExtendLegReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalExtendLeg (_, _) journeyId req = do
  JM.extendLeg journeyId req.startLocation req.endLocation Nothing req.fare req.distance req.duration req.bookingUpdateRequestId [] []
  return Kernel.Types.APISuccess.Success

postMultimodalExtendLegGetfare ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.ExtendLegGetFareReq ->
  Environment.Flow ApiTypes.ExtendLegGetFareResp
postMultimodalExtendLegGetfare (_, _) journeyId req = do
  JM.extendLegEstimatedFare journeyId req.startLocation req.endLocation Nothing

-- TODO :: To be deprecated from UI @Khuzema: Call cancel instead of this API
postMultimodalJourneyLegSkip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Int ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegSkip (_, _) journeyId legOrder = do
  journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
  JM.cancelLeg journeyLeg (SCR.CancellationReasonCode "") False Nothing
  pure Kernel.Types.APISuccess.Success

-- TODO :: To be deprecated from UI @Khuzema: Call confirm instead of this API
postMultimodalJourneyLegAddSkippedLeg ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Int ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegAddSkippedLeg (_, _) journeyId legOrder = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsInfo journey.riderId journeyId
  leg <- find (\leg -> leg.order == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  void $ JM.startJourneyLeg leg journey.isSingleMode
  pure Kernel.Types.APISuccess.Success

getMultimodalJourneyStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatusResp
getMultimodalJourneyStatus (_, _) journeyId = do
  (journeyStatusResp, _) <- getMultimodalJourneyStatusAndLegs journeyId
  return journeyStatusResp

getMultimodalJourneyStatusAndLegs ::
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow (ApiTypes.JourneyStatusResp, [JMTypes.JourneyLegState])
getMultimodalJourneyStatusAndLegs journeyId = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsStatus journey
  (,legs) <$> generateJourneyStatusResponse journey legs

postMultimodalJourneyFeedback :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> API.Types.UI.MultimodalConfirm.JourneyFeedBackForm -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyFeedback (mbPersonId, merchantId) journeyId journeyFeedbackForm = do
  journey <- JM.getJourney journeyId
  riderId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  whenJust journeyFeedbackForm.rating $ \rating -> do
    let mkJourneyfeedbackForm =
          JFB.JourneyFeedback
            { additionalFeedBack = journeyFeedbackForm.additionalFeedBack,
              journeyId = journeyId,
              rating = Just rating,
              riderId = riderId,
              merchantId = Just merchantId,
              merchantOperatingCityId = Just journey.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
    SQJFB.create mkJourneyfeedbackForm
    JM.updateJourneyStatus journey Domain.Types.Journey.COMPLETED

  legs <- QJourneyLeg.getJourneyLegs journeyId
  mapM_
    ( \legFeedback -> do
        journeyLeg <- find (\leg -> leg.sequenceNumber == legFeedback.legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
        case journeyLeg.mode of
          DTrip.Taxi -> do
            mbBooking <- maybe (pure Nothing) QBooking.findByTransactionId journeyLeg.legSearchId
            mbRide <- maybe (pure Nothing) (QRide.findOneByBookingId . (.id)) mbBooking
            let mbRatingValue = legFeedback.rating <|> maybe Nothing (bool (Just 1) (Just 5)) legFeedback.isExperienceGood
            case (mbRide, mbRatingValue) of
              (Just ride, Just ratingValue) -> do
                let feedbackReq = Rating.FeedbackReq {rideId = ride.id, rating = ratingValue, wasRideSafe = Nothing, shouldFavDriver = Nothing, wasOfferedAssistance = Nothing, feedbackDetails = journeyFeedbackForm.additionalFeedBack, mbAudio = Nothing, feedbackAnswers = Nothing}
                withTryCatch "processRating:postFeedbackJourney" (Rating.processRating (riderId, merchantId) feedbackReq)
                  >>= \case
                    Right _ -> pure ()
                    Left err -> do
                      logError $ "Error in rating the ride: " <> show err
                      pure ()
              _ -> pure ()
          DTrip.Metro -> frfsFeedback journeyLeg legFeedback
          DTrip.Subway -> frfsFeedback journeyLeg legFeedback
          DTrip.Bus -> frfsFeedback journeyLeg legFeedback
          _ -> pure ()
    )
    journeyFeedbackForm.rateTravelMode
  pure Kernel.Types.APISuccess.Success
  where
    frfsFeedback journeyLeg legFeedback = do
      mbBooking <- maybe (pure Nothing) (QFRFSTicketBooking.findBySearchId . Id) journeyLeg.legSearchId
      let feedbackDetails = if legFeedback.isExperienceGood == Just True then "Good Experience" else "Bad Experience"
      case mbBooking of
        Just booking -> do
          withTryCatch "postFrfsBookingFeedback:postFeedbackJourney" (FRFSTicketService.postFrfsBookingFeedback (mbPersonId, merchantId) booking.id (FRFSTicketService.BookingFeedback FRFSTicketService.BookingFeedbackReq {feedbackDetails = feedbackDetails}))
            >>= \case
              Right _ -> pure ()
              Left err -> do
                logError $ "Error in rating the frfs booking: " <> show err
                pure ()
        _ -> pure ()

getMultimodalFeedback :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Environment.Flow (Kernel.Prelude.Maybe ApiTypes.JourneyFeedBackForm)
getMultimodalFeedback (mbPersonId, _) journeyId = do
  _ <- JM.getJourney journeyId
  _ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbFeedBackForjourney <- SQJFB.findByJourneyId journeyId
  case mbFeedBackForjourney of
    Just feedBackForjourney -> do
      legs <- QJourneyLeg.getJourneyLegs journeyId
      let ratingForLegs = map mkRatingForLegs legs
      return $ Just (mkFeedbackFormData feedBackForjourney ratingForLegs)
    Nothing -> return Nothing
  where
    mkFeedbackFormData :: JFB.JourneyFeedback -> [ApiTypes.RateMultiModelTravelModes] -> ApiTypes.JourneyFeedBackForm
    mkFeedbackFormData feedBackForjourney ratingForLegs =
      ApiTypes.JourneyFeedBackForm
        { rating = feedBackForjourney.rating,
          additionalFeedBack = feedBackForjourney.additionalFeedBack,
          rateTravelMode = ratingForLegs
        }

    mkRatingForLegs :: DJourneyLeg.JourneyLeg -> ApiTypes.RateMultiModelTravelModes
    mkRatingForLegs journeyLeg =
      ApiTypes.RateMultiModelTravelModes
        { isExperienceGood = Nothing, -- fix this
          legOrder = journeyLeg.sequenceNumber,
          travelMode = Just journeyLeg.mode,
          rating = Nothing
        }

getMultimodalUserPreferences ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.MultimodalConfirm.MultimodalUserPreferences
  )
getMultimodalUserPreferences (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  multimodalUserPreferences <- QMP.findByPersonId personId
  --TODO: handle case when post updates this to almost blank case
  case multimodalUserPreferences of
    Just multimodalUserPreferences' ->
      return $
        ApiTypes.MultimodalUserPreferences
          { allowedTransitModes = multimodalUserPreferences'.allowedTransitModes,
            journeyOptionsSortingType = Just multimodalUserPreferences'.journeyOptionsSortingType,
            busTransitTypes = multimodalUserPreferences'.busTransitTypes,
            subwayTransitTypes = multimodalUserPreferences'.subwayTransitTypes
          }
    Nothing -> do
      personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
      riderConfig <- QRiderConfig.findByMerchantOperatingCityId personCityInfo.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound personCityInfo.merchantOperatingCityId.getId)
      let convertedModes = mapMaybe generalVehicleTypeToAllowedTransitMode (fromMaybe [] riderConfig.permissibleModes)
          journeyOptionsSortingType = fromMaybe DMP.MOST_RELEVANT riderConfig.journeyOptionsSortingType
          busTransitTypes = fromMaybe [Spec.ORDINARY, Spec.EXPRESS, Spec.SPECIAL, Spec.AC, Spec.NON_AC, Spec.EXECUTIVE] riderConfig.busTransitTypes
          subwayTransitTypes = fromMaybe [Spec.FIRST_CLASS, Spec.SECOND_CLASS] riderConfig.subwayTransitTypes
      return $
        ApiTypes.MultimodalUserPreferences
          { allowedTransitModes = convertedModes <> [DTrip.Taxi],
            journeyOptionsSortingType = Just journeyOptionsSortingType,
            busTransitTypes = Just busTransitTypes,
            subwayTransitTypes = Just subwayTransitTypes
          }
  where
    generalVehicleTypeToAllowedTransitMode :: GeneralVehicleType -> Maybe DTrip.MultimodalTravelMode
    generalVehicleTypeToAllowedTransitMode vehicleType = case vehicleType of
      MultiModalTypes.Bus -> Just DTrip.Bus
      MultiModalTypes.MetroRail -> Just DTrip.Metro
      MultiModalTypes.Subway -> Just DTrip.Subway
      MultiModalTypes.Walk -> Just DTrip.Walk
      _ -> Nothing

postMultimodalUserPreferences ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultimodalConfirm.MultimodalUserPreferences ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalUserPreferences (mbPersonId, merchantId) multimodalUserPreferences = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  existingPreferences <- QMP.findByPersonId personId
  let updatedAllowedModes =
        if DTrip.Walk `elem` multimodalUserPreferences.allowedTransitModes
          then multimodalUserPreferences.allowedTransitModes
          else DTrip.Walk : multimodalUserPreferences.allowedTransitModes
  case existingPreferences of
    Just _ -> do
      QMP.updateUserPreferences updatedAllowedModes (fromMaybe DMP.FASTEST multimodalUserPreferences.journeyOptionsSortingType) multimodalUserPreferences.busTransitTypes multimodalUserPreferences.subwayTransitTypes personId
    Nothing -> do
      now <- getCurrentTime
      let newPreferences =
            MultimodalPreferences
              { allowedTransitModes = updatedAllowedModes,
                journeyOptionsSortingType = fromMaybe DMP.FASTEST multimodalUserPreferences.journeyOptionsSortingType,
                busTransitTypes = multimodalUserPreferences.busTransitTypes,
                subwayTransitTypes = multimodalUserPreferences.subwayTransitTypes,
                personId = personId,
                merchantId = Just merchantId,
                merchantOperatingCityId = Just personCityInfo.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QMP.create newPreferences
  pure Kernel.Types.APISuccess.Success

-- TODO :: To be deprecated from UI @Khuzema
postMultimodalTransitOptionsLite ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultimodalConfirm.MultimodalTransitOptionsReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.MultimodalTransitOptionsResp
  )
postMultimodalTransitOptionsLite (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  userPreferences <- getMultimodalUserPreferences (mbPersonId, merchantId)
  JM.getMultiModalTransitOptions userPreferences merchantId personCityInfo.merchantOperatingCityId req

getPublicTransportVehicleData ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    VehicleCategory ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.MultimodalConfirm.PublicTransportData
  )
getPublicTransportVehicleData (mbPersonId, merchantId) vehicleType vehicleNumber = do
  case vehicleType of
    BUS -> getPublicTransportDataImpl (mbPersonId, merchantId) Nothing (Just True) Nothing (Just vehicleNumber) (Just BUS) True
    _ -> throwError (InvalidRequest $ "Invalid vehicle type: " <> show vehicleType)

type CacheKey = (Text, Maybe Text, Maybe Bool, Maybe Text, Maybe Text, Bool)

type CacheValue = (Text, ApiTypes.PublicTransportData)

type PublicTransportCache = MVar (HashMap.HashMap CacheKey CacheValue)

{-# NOINLINE publicTransportCache #-}
publicTransportCache :: PublicTransportCache
publicTransportCache = unsafePerformIO $ newMVar HashMap.empty

computeCacheKey ::
  Id DMOC.MerchantOperatingCity ->
  Maybe Kernel.Types.Beckn.Context.City ->
  Maybe Bool ->
  Maybe Text ->
  Maybe VehicleCategory ->
  Bool ->
  CacheKey
computeCacheKey merchantOperatingCityId mbCity mbEnableSwitchRoute mbVehicleNumber mbVehicleType isPublicVehicleData =
  ( merchantOperatingCityId.getId,
    show <$> mbCity,
    mbEnableSwitchRoute,
    mbVehicleNumber,
    show <$> mbVehicleType,
    isPublicVehicleData
  )

computeVersionFromConfigs ::
  [DIBC.IntegratedBPPConfig] ->
  Maybe DRC.RiderConfig ->
  Environment.Flow Text
computeVersionFromConfigs integratedBPPConfigs mbRiderConfig = do
  gtfsVersion <-
    withTryCatch "getGtfsVersion:computeVersionFromConfigs" (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
      Left _ -> return (map (.feedKey) integratedBPPConfigs)
      Right gtfsVersions -> return gtfsVersions
  return $ T.intercalate (T.pack "#") gtfsVersion <> (maybe "" (\version -> "#" <> show version) (mbRiderConfig >>= (.domainPublicTransportDataVersion)))

getCachedPublicTransportData ::
  CacheKey ->
  Text ->
  Environment.Flow ApiTypes.PublicTransportData ->
  Environment.Flow ApiTypes.PublicTransportData
getCachedPublicTransportData cacheKey currentVersion computeData = do
  cacheMap <- liftIO $ readMVar publicTransportCache
  case HashMap.lookup cacheKey cacheMap of
    Just (cachedVersion, cachedData)
      | cachedVersion == currentVersion ->
        return cachedData
    _ -> do
      _ <- liftIO $
        modifyMVar publicTransportCache $ \cacheMap' ->
          return (HashMap.delete cacheKey cacheMap', ())

      newData <- computeData

      _ <- liftIO $
        modifyMVar publicTransportCache $ \cacheMap' ->
          return (HashMap.insert cacheKey (currentVersion, newData) cacheMap', ())

      return newData

-- todo: segregate these APIs properly.

getPublicTransportData ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe VehicleCategory ->
    Environment.Flow API.Types.UI.MultimodalConfirm.PublicTransportData
  )
getPublicTransportData (mbPersonId, merchantId) mbCity mbEnableSwitchRoute _mbConfigVersion mbVehicleNumber mbVehicleType = do
  -- Get parameters needed for cache key and version computation
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbRequestCity <- maybe (pure Nothing) (CQMOC.findByMerchantIdAndCity merchantId) mbCity
  let merchantOperatingCityId = maybe person.merchantOperatingCityId (.id) mbRequestCity
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId
  let vehicleTypes =
        case mbVehicleType of
          Just BUS -> [Enums.BUS]
          Just METRO -> [Enums.METRO]
          Just SUBWAY -> [Enums.SUBWAY]
          _ -> [Enums.BUS, Enums.METRO, Enums.SUBWAY]

  -- Get integratedBPPConfigs to compute version
  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes

  -- Compute version from integratedBPPConfigs
  currentVersion <- computeVersionFromConfigs integratedBPPConfigs riderConfig

  -- Compute cache key
  let cacheKey = computeCacheKey merchantOperatingCityId mbCity mbEnableSwitchRoute mbVehicleNumber mbVehicleType False

  -- Get from cache or compute and store
  getCachedPublicTransportData cacheKey currentVersion $
    getPublicTransportDataImpl (mbPersonId, merchantId) mbCity mbEnableSwitchRoute _mbConfigVersion mbVehicleNumber mbVehicleType False

getPublicTransportDataImpl ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe VehicleCategory ->
    Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.MultimodalConfirm.PublicTransportData
  )
getPublicTransportDataImpl (mbPersonId, merchantId) mbCity mbEnableSwitchRoute _mbConfigVersion mbVehicleNumber mbVehicleType isPublicVehicleData = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mbRequestCity <- maybe (pure Nothing) (CQMOC.findByMerchantIdAndCity merchantId) mbCity
  let merchantOperatingCityId = maybe person.merchantOperatingCityId (.id) mbRequestCity
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId
  let vehicleTypes =
        case mbVehicleType of
          Just BUS -> [Enums.BUS]
          Just METRO -> [Enums.METRO]
          Just SUBWAY -> [Enums.SUBWAY]
          _ -> [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  fork "incrementBusScannetCounterMetric" $
    whenJust mbVehicleNumber $ \vehicleNumber -> do
      Metrics.incrementBusScannetCounterMetric merchant.shortId.getShortId merchantOperatingCityId.getId vehicleNumber

  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes
  mbVehicleLiveRouteInfo <-
    case mbVehicleNumber of
      Just vehicleNumber -> do
        mbVehicleOverrideInfo <- Dispatcher.getFleetOverrideInfo vehicleNumber
        case mbVehicleOverrideInfo of
          Just (updatedVehicleNumber, newDeviceWaybillNo) -> do
            updatedVehicleRouteInfo <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs updatedVehicleNumber Nothing >>= fromMaybeM (InvalidVehicleNumber $ "Vehicle override: " <> updatedVehicleNumber <> " for vehicle: " <> vehicleNumber <> ", not found on any route")
            if Just newDeviceWaybillNo /= (snd updatedVehicleRouteInfo).waybillId
              then do
                Dispatcher.delFleetOverrideInfo vehicleNumber
                getVehicleLiveRouteInfo vehicleNumber integratedBPPConfigs
              else pure $ Just updatedVehicleRouteInfo
          Nothing -> getVehicleLiveRouteInfo vehicleNumber integratedBPPConfigs
      Nothing -> return Nothing

  -- eligiblePassIds are vehicle-level data.
  -- They must be present only when live vehicle info exists.
  let mbEligiblePassIds =
        mbVehicleLiveRouteInfo >>= (JMU.eligiblePassIds . snd)

  let mbOppositeTripDetails :: Maybe [NandiTypes.BusScheduleTrip] =
        case (mbEnableSwitchRoute, isPublicVehicleData) of
          (Just True, False) -> do
            mbVehicleLiveRouteInfo
              <&> \(_, vehicleLiveRouteInfo) -> do
                ( \tripsSortedOnStopCount ->
                    if vehicleLiveRouteInfo.tripNumber == Just 1
                      then do
                        let nextTrip = take 1 tripsSortedOnStopCount
                        let nextUniqueTrip = take 1 $ filter (\t -> not $ t.route_id `elem` map (.route_id) nextTrip) tripsSortedOnStopCount
                        nextTrip ++ nextUniqueTrip
                      else take 1 tripsSortedOnStopCount
                  )
                  $ sortOn (Down . (.stops_count)) $ filter (\remainingTrip -> remainingTrip.route_number == vehicleLiveRouteInfo.routeNumber && Just remainingTrip.route_id /= vehicleLiveRouteInfo.routeCode) (fromMaybe [] vehicleLiveRouteInfo.remaining_trip_details)
          (Just True, True) -> mbVehicleLiveRouteInfo >>= \(_, vehicleLiveRouteInfo) -> vehicleLiveRouteInfo.remaining_trip_details
          _ -> Nothing

  busStationListHackEnabled <- liftIO $ fromMaybe False . (>>= readMaybe) <$> lookupEnv "BUS_STATION_LIST_HACK_ENABLED"

  let mkResponse stations routes routeStops bppConfig mbServiceType = do
        frfsServiceTier <- maybe (pure Nothing) (\serviceType -> CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceType person.merchantOperatingCityId bppConfig.id) mbServiceType
        gtfsVersion <-
          withTryCatch "getGtfsVersion:mkResponse" (OTPRest.getGtfsVersion bppConfig) >>= \case
            Left _ -> return bppConfig.feedKey
            Right gtfsVersion -> return gtfsVersion
        -- Get service subtypes from external API for specific vehicle
        let mbServiceSubTypes = do
              vehicleRouteInfo <- mbVehicleLiveRouteInfo
              let vehicleLiveInfo = snd vehicleRouteInfo
              vehicleLiveInfo.serviceSubTypes
        pure
          ApiTypes.PublicTransportData
            { ss =
                mapMaybe
                  ( \s -> case (s.lat, s.lon) of
                      (Just lat, Just lon) ->
                        Just
                          ApiTypes.TransportStation
                            { cd = s.code,
                              nm = s.name,
                              lt = lat,
                              ln = lon,
                              ad = s.address,
                              vt = show s.vehicleType,
                              rgn = s.regionalName,
                              sgstdDest = s.suggestedDestinations,
                              hin = s.hindiName,
                              gj = s.geoJson,
                              gi = s.gates,
                              ibc = bppConfig.id
                            }
                      _ -> Nothing
                  )
                  stations,
              rs =
                map
                  ( \r ->
                      ApiTypes.TransportRoute
                        { cd = r.code,
                          sN = r.shortName,
                          lN = r.longName,
                          dTC = r.dailyTripCount,
                          stC = r.stopCount,
                          st = mbServiceType,
                          stn = frfsServiceTier <&> (.shortName),
                          sst = mbServiceSubTypes,
                          vt = show r.vehicleType,
                          clr = r.color,
                          ibc = bppConfig.id
                        }
                  )
                  routes,
              rsm =
                map
                  ( \rsm ->
                      ApiTypes.TransportRouteStopMapping
                        { rc = rsm.routeCode,
                          sc = rsm.stopCode,
                          sn = rsm.sequenceNum,
                          ibc = bppConfig.id
                        }
                  )
                  routeStops,
              ptcv = gtfsVersion,
              eligiblePassIds = Nothing
            }

  let fetchData mbRouteCodeAndServiceType bppConfig = do
        case mbRouteCodeAndServiceType of
          Just (routeCode, mbServiceType) -> do
            withTryCatch
              "getPublicTransportData:fetchData"
              ( do
                  routes <- maybeToList <$> OTPRest.getRouteByRouteId bppConfig routeCode
                  routeStopMappingInMem <- OTPRest.getRouteStopMappingByRouteCodeInMem routeCode bppConfig
                  routeStops <- OTPRest.parseRouteStopMappingInMemoryServer routeStopMappingInMem bppConfig bppConfig.merchantId bppConfig.merchantOperatingCityId
                  stations <- OTPRest.parseStationsFromInMemoryServer routeStopMappingInMem bppConfig (not isPublicVehicleData)
                  mkResponse stations routes routeStops bppConfig mbServiceType
              )
              >>= \case
                Left err -> throwError (PublicTransportDataUnavailable $ "Public transport data unavailable: " <> show err)
                Right response -> return response
          Nothing -> do
            stations <- OTPRest.getStationsByGtfsId bppConfig
            routes <- OTPRest.getRoutesByGtfsId bppConfig
            let (finalStations, finalRoutes) =
                  -- hack to solve the dummy trips and stations, route added for spot booking in GTFS to not appear in single mode search.
                  if bppConfig.vehicleCategory == Enums.BUS && busStationListHackEnabled
                    then (filter (\s -> isJust s.address) stations, routes)
                    else (stations, routes)
            mkResponse finalStations finalRoutes ([] :: [DRSM.RouteStopMapping]) bppConfig Nothing

  transportDataList <-
    withTryCatch
      "getOTPRestServiceReq:getPublicTransportData"
      ( mapM
          ( \config -> do
              baseUrl <- MM.getOTPRestServiceReq config.merchantId config.merchantOperatingCityId
              return (config, (config.feedKey, baseUrl))
          )
          integratedBPPConfigs
      )
      >>= \case
        Left _ -> do
          mbRouteCodeAndServiceType :: Maybe (Kernel.Prelude.Text, Maybe Spec.ServiceTierType) <- getRouteCodeAndServiceType mbVehicleLiveRouteInfo merchant merchantOperatingCityId
          lst1 <- mapConcurrently (fetchData mbRouteCodeAndServiceType) integratedBPPConfigs
          lst2 <-
            maybe
              (pure [])
              ( \oppositeTripDetails ->
                  concat
                    <$> mapConcurrently
                      (\oppositeTripDetail -> mapConcurrently (fetchData (Just (oppositeTripDetail.route_id, snd =<< mbRouteCodeAndServiceType))) integratedBPPConfigs)
                      oppositeTripDetails
              )
              mbOppositeTripDetails
          return (lst1 <> lst2)
        Right configsWithFeedInfo -> do
          -- Group configs by feed_id and take first config for each feed_id
          let configsByFeedId = HashMap.fromListWith (++) $ map (\(config, (feedKey, _)) -> (feedKey, [config])) configsWithFeedInfo
              uniqueConfigs = map (head . snd) $ HashMap.toList configsByFeedId
          mbRouteCodeAndServiceType <- getRouteCodeAndServiceType mbVehicleLiveRouteInfo merchant merchantOperatingCityId
          lst1 <- mapConcurrently (fetchData mbRouteCodeAndServiceType) uniqueConfigs
          lst2 <- maybe (pure []) (\oppositeTripDetails -> concat <$> mapConcurrently (\oppositeTripDetail -> mapConcurrently (fetchData (Just (oppositeTripDetail.route_id, snd =<< mbRouteCodeAndServiceType))) uniqueConfigs) oppositeTripDetails) mbOppositeTripDetails
          return (lst1 <> lst2)

  gtfsVersion <-
    withTryCatch "getGtfsVersion:getPublicTransportData" (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
      Left _ -> return (map (.feedKey) integratedBPPConfigs)
      Right gtfsVersions -> return gtfsVersions
  let transportData =
        ApiTypes.PublicTransportData
          { ss = concatMap (.ss) transportDataList,
            rs = concatMap (.rs) transportDataList,
            rsm = concatMap (.rsm) transportDataList,
            ptcv = T.intercalate (T.pack "#") gtfsVersion <> (maybe "" (\version -> "#" <> show version) (riderConfig >>= (.domainPublicTransportDataVersion))),
            eligiblePassIds = mbEligiblePassIds
          }
  return transportData
  where
    getVehicleLiveRouteInfo vehicleNumber integratedBPPConfigs = do
      vehicleRouteInfo <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber Nothing >>= fromMaybeM (InvalidVehicleNumber $ "Vehicle " <> vehicleNumber <> ", not found on any route")
      pure $ Just vehicleRouteInfo

    getRouteCodeAndServiceType Nothing _ _ = return Nothing
    getRouteCodeAndServiceType (Just vehicleLiveRouteInfo) merchant merchantOperatingCityId = do
      let mbRouteCode = (snd vehicleLiveRouteInfo).routeCode
      case mbRouteCode of
        Just routeCode -> return $ Just (routeCode, Just ((snd vehicleLiveRouteInfo).serviceType))
        Nothing -> do
          let vehicleNumber = (snd vehicleLiveRouteInfo).vehicleNumber
          fork "incrementFleetRouteMapMissingCounter" $
            Metrics.incrementFleetRouteMapMissingCounter merchant.shortId.getShortId merchantOperatingCityId.getId vehicleNumber
          throwError (FleetRouteMapMissing $ "Route code not found for fleetId: " <> show vehicleNumber)

getMultimodalOrderGetLegTierOptions ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Environment.Flow ApiTypes.LegServiceTierOptionsResp
getMultimodalOrderGetLegTierOptions (_mbPersonId, _merchantId) journeyId legOrder mbEnableSuburbanRoundTrip = do
  legs <- QJourneyLeg.getJourneyLegs journeyId
  journeyLegInfo <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let enableSuburbanRoundTrip = fromMaybe False mbEnableSuburbanRoundTrip
  options <- getLegTierOptions journeyLegInfo enableSuburbanRoundTrip
  return $ ApiTypes.LegServiceTierOptionsResp {options}

-- TODO :: For Backward compatibility, remove this post `postMultimodalOrderSublegSetTrackingStatus` release
postMultimodalOrderSublegSetStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Int ->
  Kernel.Prelude.Int ->
  JL.JourneyLegStatus ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalOrderSublegSetStatus (_, _) journeyId legOrder subLegOrder newStatus = do
  journey <- JM.getJourney journeyId
  now <- getCurrentTime

  legs <- QJourneyLeg.getJourneyLegs journeyId

  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")

  markLegStatus (Just newStatus) Nothing journeyLeg (Just subLegOrder) now

  -- refetch updated legs and journey
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey updatedLegStatus
  updatedJourney <- JM.getJourney journeyId
  generateJourneyStatusResponse updatedJourney updatedLegStatus

postMultimodalOrderSublegSetTrackingStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Int ->
  Kernel.Prelude.Int ->
  JMState.TrackingStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalOrderSublegSetTrackingStatus (_, _) journeyId legOrder subLegOrder trackingStatus mbTrackingStatusUpdateTime = do
  now <- getCurrentTime
  -- In case user device time is of future, consider the updated time to be of now to avoid backend update miss
  let trackingStatusUpdateTime =
        case mbTrackingStatusUpdateTime of
          Just t
            | t > now -> now
            | otherwise -> t
          Nothing -> now
  journey <- JM.getJourney journeyId

  legs <- QJourneyLeg.getJourneyLegs journeyId

  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")

  markLegStatus Nothing (Just trackingStatus) journeyLeg (Just subLegOrder) trackingStatusUpdateTime

  -- refetch updated legs and journey
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey updatedLegStatus
  updatedJourney <- JM.getJourney journeyId
  generateJourneyStatusResponse updatedJourney updatedLegStatus

postMultimodalTicketVerify ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.MultimodalConfirm.MultimodalTicketVerifyReq ->
  Environment.Flow API.Types.UI.MultimodalConfirm.MultimodalTicketVerifyResp
postMultimodalTicketVerify (_mbPersonId, merchantId) opCity req = do
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show opCity)
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchantId (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory BUS) >>= fromMaybeM (InternalError "Beckn Config not found")
  let verifyTicketsAndBuildResponse provider tickets = do
        legInfoList <- forM tickets $ \ticketQR -> do
          ticket <- CallExternalBPP.verifyTicket merchantId merchantOperatingCity bapConfig BUS ticketQR DIBC.MULTIMODAL
          booking <- QFRFSTicketBooking.findById ticket.frfsTicketBookingId >>= fromMaybeM (BookingNotFound ticket.frfsTicketBookingId.getId)
          journeyLeg <- QJourneyLeg.findByLegSearchId (Just booking.searchId.getId) >>= fromMaybeM (JourneyLegNotFound booking.searchId.getId)
          JMTypes.mkLegInfoFromFrfsBooking booking journeyLeg
        return $
          API.Types.UI.MultimodalConfirm.MultimodalTicketVerifyResp
            { provider = provider,
              legInfo = legInfoList
            }

  case req of
    ApiTypes.IntegratedQR integratedQRData -> do
      case integratedQRData.provider of
        JMTypes.MTC -> do
          let allTickets = concatMap (.ticketData) integratedQRData.integratedQR.mtc
          verifyTicketsAndBuildResponse integratedQRData.provider allTickets
        JMTypes.CMRL -> throwError $ InvalidRequest "CMRL provider not implemented yet"
        JMTypes.CRIS -> throwError $ InvalidRequest "CRIS provider not implemented yet"
        _ -> throwError $ InvalidRequest "Invalid provider"
    ApiTypes.SingleQR singleQRData -> do
      case singleQRData.provider of
        JMTypes.MTC -> verifyTicketsAndBuildResponse singleQRData.provider singleQRData.tickets
        JMTypes.CMRL -> throwError $ InvalidRequest "CMRL provider not implemented yet"
        JMTypes.CRIS -> throwError $ InvalidRequest "CRIS provider not implemented yet"
        _ -> throwError $ InvalidRequest "Invalid provider"

postMultimodalComplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalComplete (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  updatedLegStatus <- JM.markJourneyComplete journey legs
  updatedJourney <- JM.getJourney journeyId
  generateJourneyStatusResponse updatedJourney updatedLegStatus

postMultimodalOrderSoftCancel ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalOrderSoftCancel (_, _) journeyId legOrder = do
  legs <- QJourneyLeg.getJourneyLegs journeyId
  checkIfAnyTaxiLegOngoing legs -- check for any ongoing taxi legs, remove this once handled properly from UI
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  JM.softCancelLeg journeyLeg (SCR.CancellationReasonCode "") False Nothing
  return Kernel.Types.APISuccess.Success

getMultimodalOrderCancelStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Environment.Flow API.Types.UI.MultimodalConfirm.MultimodalCancelStatusResp
  )
getMultimodalOrderCancelStatus (_, __) journeyId legOrder = do
  journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
  -- TODO: Move to the JourneyLeg class
  ticketBooking <- maybe (pure Nothing) (QFRFSTicketBooking.findBySearchId . Id) journeyLeg.legSearchId >>= fromMaybeM (InvalidRequest "No FRFS booking found for the leg")
  return $
    ApiTypes.MultimodalCancelStatusResp
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = getAbsoluteValue ticketBooking.refundAmount,
        isCancellable = ticketBooking.isBookingCancellable,
        bookingStatus = ticketBooking.status
      }

postMultimodalOrderCancel ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalOrderCancel (_, _) journeyId legOrder = do
  journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
  legs <- QJourneyLeg.getJourneyLegs journeyId
  cancelOngoingTaxiLegs legs -- shouldn't be there once we have leg wise cancellation
  JM.cancelLeg journeyLeg (SCR.CancellationReasonCode "") False Nothing
  return Kernel.Types.APISuccess.Success

getAbsoluteValue :: Maybe HighPrecMoney -> Maybe HighPrecMoney
getAbsoluteValue mbRefundAmount = case mbRefundAmount of
  Nothing -> Nothing
  Just rfValue -> do
    let HighPrecMoney value = rfValue
    Just (HighPrecMoney $ abs value)

getMultimodalOrderSimilarJourneyLegs ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Environment.Flow API.Types.UI.MultimodalConfirm.SimilarJourneyLegsResp
  )
getMultimodalOrderSimilarJourneyLegs (mbPersonId, merchantId) journeyId legOrder = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  mbSearchRequest <- QSearchRequest.findById (Id journey.searchRequestId)
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let groupCode = JMTypes.mkJourneyLegGroupCode (Id journey.searchRequestId) journeyLeg.mode journeyLeg.fromStopDetails journeyLeg.toStopDetails
  journeyLegs <- QJourneyLeg.findByGroupCode groupCode
  let allLegsLoaded = fromMaybe True (mbSearchRequest >>= (.allJourneysLoaded))
  let journeyLegsWithTransits = map (\leg -> (mkStationCodesFromRouteDetails leg.routeDetails, leg)) journeyLegs
      sortedJourneyLegsWithTransits = sortBy (\a b -> compare (fst a) (fst b)) journeyLegsWithTransits
      uniqueJourneyLegsWithTransits = nubBy (\a b -> fst a == fst b) sortedJourneyLegsWithTransits
  journeyLegsInfo <- mapMaybeM (createLegOption person . snd) uniqueJourneyLegsWithTransits
  return $ API.Types.UI.MultimodalConfirm.SimilarJourneyLegsResp {journeyLegsInfo, allLegsLoaded}
  where
    mkStationCodesFromRouteDetails :: [RD.RouteDetails] -> Text
    mkStationCodesFromRouteDetails routeDetails =
      let stationCodes = catMaybes (map (.fromStopCode) routeDetails <> maybe [] (\el -> [el.toStopCode]) (JMTypes.safeTail routeDetails))
       in T.intercalate "-" stationCodes

    createLegOption person leg = do
      now <- getCurrentTime
      let vehicleCategory = castTravelModeToVehicleCategory leg.mode
      let mbAgencyId = leg.agency >>= (.gtfsId)
      mbIntegratedBPPConfig <- SIBC.findMaybeIntegratedBPPConfigFromAgency mbAgencyId person.merchantOperatingCityId vehicleCategory DIBC.MULTIMODAL
      let mbRouteDetail = leg.routeDetails & listToMaybe
      let mbFomStopCode = mbRouteDetail >>= (.fromStopCode)
      let mbToStopCode = mbRouteDetail >>= (.toStopCode)
      let mbArrivalTime = mbRouteDetail >>= (.fromArrivalTime)
      let arrivalTime = fromMaybe now mbArrivalTime
      let changeOverStationCodes = nub $ concat $ mapMaybe (\rd -> (\x y -> [x, y]) <$> rd.fromStopCode <*> rd.toStopCode) leg.routeDetails
          changeOverPoints = case changeOverStationCodes of
            [] -> []
            [_] -> []
            xs -> nub $ drop 1 (take (length xs - 1) xs)
      mbAvailableTier <-
        case (mbFomStopCode, mbToStopCode, mbIntegratedBPPConfig) of
          (Just fromStopCode, Just toStopCode, Just integratedBPPConfig) -> do
            (_, tiers, _) <- JLU.findPossibleRoutes Nothing fromStopCode toStopCode arrivalTime integratedBPPConfig merchantId person.merchantOperatingCityId vehicleCategory True False False False
            return $ listToMaybe tiers
          _ -> return Nothing
      return $
        leg.estimatedMinFare <&> \fare ->
          JLU.JourneyLegOption
            { viaPoints = changeOverPoints,
              routeDetails = mapMaybe mkJourneyLegRouteDetails leg.routeDetails,
              arrivalTimes = maybe [] (.nextAvailableBuses) mbAvailableTier,
              availableRoutes = maybe [] (.availableRoutes) mbAvailableTier,
              fare,
              duration = leg.duration,
              distance = leg.distance,
              journeyLegId = leg.id,
              providerRouteId = leg.providerRouteId
            }

    mkJourneyLegRouteDetails :: RD.RouteDetails -> Maybe JLU.JourneyLegRouteDetails
    mkJourneyLegRouteDetails detail = do
      fromStopCode <- detail.fromStopCode
      toStopCode <- detail.toStopCode
      subLegOrder <- detail.subLegOrder
      return $ JLU.JourneyLegRouteDetails {fromStopCode, toStopCode, subLegOrder}

postMultimodalOrderSwitchJourneyLeg ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.SwitchJourneyLegReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSwitchJourneyLeg _ journeyId legOrder req = do
  journey <- JM.getJourney journeyId
  currentLegs <- QJourneyLeg.getJourneyLegs journeyId
  currentJourneyLeg <- find (\leg -> leg.sequenceNumber == legOrder) currentLegs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  switchJourneyLeg <- QJourneyLeg.findById req.journeyLegId >>= fromMaybeM (InvalidRequest "No matching journey leg found for the given journeyLegId")
  interchangeJourneyLeg currentLegs currentJourneyLeg switchJourneyLeg
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
  where
    interchangeJourneyLeg currentLegs currentJourneyLeg switchJourneyLeg = do
      currentJourneyLegMapping <- QJourneyLegMapping.findByJourneyLegId currentJourneyLeg.id >>= fromMaybeM (InvalidRequest "Journey Leg Mapping Not Found.")
      switchJourneyLegMapping <- QJourneyLegMapping.findByJourneyLegId switchJourneyLeg.id >>= fromMaybeM (InvalidRequest "Journey Leg Mapping Not Found.")
      switchLegs <- QJourneyLeg.getJourneyLegs journeyId
      QJourneyLegMapping.updateJourneyLegId switchJourneyLeg.id currentJourneyLegMapping.id
      QJourneyLegMapping.updateJourneyLegId currentJourneyLeg.id switchJourneyLegMapping.id
      updateTimings currentLegs switchJourneyLeg
      updateTimings switchLegs currentJourneyLeg
      where
        updateTimings legs journeyLeg = do
          void $
            foldrM
              ( \leg mbPrevLeg -> do
                  updatedJourneyLeg <-
                    if leg.sequenceNumber > journeyLeg.sequenceNumber
                      then do
                        let prevLegArrToArrivalTime = mbPrevLeg >>= (.toArrivalTime)
                            mbCurrentLegFromArrivalTime = prevLegArrToArrivalTime
                            mbCurrentLegFromDepartureTime =
                              ((,,) <$> leg.fromArrivalTime <*> leg.fromDepartureTime <*> mbCurrentLegFromArrivalTime) <&> \(fromArrivalTime, fromDepartureTime, currentLegFromArrivalTime) -> addUTCTime (diffUTCTime fromArrivalTime fromDepartureTime) currentLegFromArrivalTime
                            mbCurrentLegToArrivalTime =
                              ((,,) <$> leg.fromDepartureTime <*> leg.toArrivalTime <*> mbCurrentLegFromDepartureTime) <&> \(fromDepartureTime, toArrivalTime, currentLegFromDepartureTime) -> addUTCTime (diffUTCTime fromDepartureTime toArrivalTime) currentLegFromDepartureTime
                            mbCurrentLegToDepartureTime =
                              ((,,) <$> leg.toArrivalTime <*> leg.toDepartureTime <*> mbCurrentLegToArrivalTime) <&> \(toArrivalTime, toDepartureTime, currentLegToArrivalTime) -> addUTCTime (diffUTCTime toArrivalTime toDepartureTime) currentLegToArrivalTime
                            journeyLeg' =
                              leg{DJourneyLeg.fromArrivalTime = mbCurrentLegFromArrivalTime,
                                  DJourneyLeg.fromDepartureTime = mbCurrentLegFromDepartureTime,
                                  DJourneyLeg.toArrivalTime = mbCurrentLegToArrivalTime,
                                  DJourneyLeg.toDepartureTime = mbCurrentLegToDepartureTime
                                 }
                        QJourneyLeg.updateByPrimaryKey journeyLeg'
                        return journeyLeg'
                      else return leg
                  return (Just updatedJourneyLeg)
              )
              Nothing
              legs

postMultimodalSetRouteName ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultimodalConfirm.SetRouteNameReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalSetRouteName userInfo req =
  setRouteByShortName userInfo req.journeyId req.legOrder req.shortName
  where
    setRouteByShortName userInfo' journeyId legOrder shortName = do
      journey <- JM.getJourney journeyId
      journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
      unless (journeyLeg.mode == DTrip.Bus) $ throwError $ InvalidRequest "Only Bus legs supported for route name update"

      options <- getMultimodalOrderGetLegTierOptions userInfo' journeyId legOrder Nothing
      let mbMatch =
            listToMaybe $ do
              option <- options.options
              routeInfo <- option.availableRoutesInfo
              if routeInfo.shortName == shortName then [routeInfo] else []

      case mbMatch of
        Nothing -> throwError $ InvalidRequest "Invalid route short name for the leg"
        Just routeInfo -> do
          QRouteDetails.updateRoute (Just routeInfo.routeCode) (Just routeInfo.routeCode) (Just routeInfo.longName) (Just routeInfo.shortName) journeyLeg.id.getId
          updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
          generateJourneyInfoResponse journey updatedLegs

postMultimodalOrderChangeStops ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.ChangeStopsReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.ChangeStopsResp
  )
postMultimodalOrderChangeStops _ journeyId legOrder req = do
  when (isNothing req.newSourceStation && isNothing req.newDestinationStation) $
    throwError $ InvalidStationChange "" "At least one of newSourceStation or newDestinationStation must be provided"
  journey <- JM.getJourney journeyId
  allLegs <- QJourneyLeg.getJourneyLegs journeyId
  reqJourneyLeg <- find (\leg -> leg.sequenceNumber == legOrder) allLegs & fromMaybeM (InvalidLegOrder legOrder)
  validateChangeNeededForStop reqJourneyLeg req.newSourceStation req.newDestinationStation
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing reqJourneyLeg.merchantOperatingCityId (fromMaybe Enums.METRO $ JM.multiModalTravelModeToBecknVehicleCategory reqJourneyLeg.mode) DIBC.MULTIMODAL
  riderConfig <-
    QRC.findByMerchantOperatingCityId reqJourneyLeg.merchantOperatingCityId Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist reqJourneyLeg.merchantOperatingCityId.getId)

  (newLeg, mbSourceStation, mbDestStation) <- processChangeStops journey reqJourneyLeg integratedBPPConfig riderConfig

  let (prevLeg, nextLeg) = JMU.findAdjacentLegs reqJourneyLeg.sequenceNumber allLegs
  mbGates <- updateLegsWithGates riderConfig prevLeg nextLeg reqJourneyLeg newLeg journey.merchantId reqJourneyLeg.merchantOperatingCityId mbSourceStation mbDestStation

  let finalBoardedBus =
        JMTypes.FinalBoardedBusData
          { busNumber = reqJourneyLeg.finalBoardedBusNumber,
            depotNo = reqJourneyLeg.finalBoardedDepotNo,
            waybillId = reqJourneyLeg.finalBoardedWaybillId,
            scheduleNo = reqJourneyLeg.finalBoardedScheduleNo,
            updateSource = reqJourneyLeg.finalBoardedBusNumberSource,
            serviceTierType = reqJourneyLeg.finalBoardedBusServiceTierType,
            busConductorId = reqJourneyLeg.busConductorId,
            busDriverId = reqJourneyLeg.busDriverId
          }
  newJourneyLeg <-
    JMTypes.mkJourneyLeg
      legOrder
      (Nothing, newLeg, Nothing)
      journey.fromLocation journey.toLocation
      journey.merchantId reqJourneyLeg.merchantOperatingCityId
      journeyId
      (Id journey.searchRequestId)
      riderConfig.maximumWalkDistance
      Nothing
      mbGates
      (Just finalBoardedBus)
      reqJourneyLeg.userBookedBusServiceTierType
      []

  QJourneyLegMapping.updateIsDeleted True reqJourneyLeg.id
  QJourneyLegExtra.create newJourneyLeg

  return $ API.Types.UI.MultimodalConfirm.ChangeStopsResp {stationsChanged = True}
  where
    validateChangeNeededForStop reqJourneyLeg mbSourceStation mbDestStation = do
      let currentSrc = reqJourneyLeg.fromStopDetails >>= (.stopCode)
          currentDest = reqJourneyLeg.toStopDetails >>= (.stopCode)
          newSrc = (.stopCode) <$> mbSourceStation
          newDest = (.stopCode) <$> mbDestStation
      when (currentSrc == newSrc && currentDest == newDest) $
        throwError $ InvalidStationChange "" "No change needed for the stop"

    processChangeStops ::
      (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
      Domain.Types.Journey.Journey ->
      DJourneyLeg.JourneyLeg ->
      DIBC.IntegratedBPPConfig ->
      DRC.RiderConfig ->
      m (MultiModalTypes.MultiModalLeg, Maybe DStation.Station, Maybe DStation.Station)
    processChangeStops journey reqJourneyLeg integratedBPPConfig riderConfig = do
      case (req.newSourceStation, req.newDestinationStation) of
        (Just sourceStation, Just destStation) -> do
          sourceStationData <-
            OTPRest.getStationByGtfsIdAndStopCode sourceStation.stopCode integratedBPPConfig
              >>= fromMaybeM (StationError.StationNotFound sourceStation.stopCode)
          destStationData <-
            OTPRest.getStationByGtfsIdAndStopCode destStation.stopCode integratedBPPConfig
              >>= fromMaybeM (StationError.StationNotFound destStation.stopCode)

          sourceLat <- sourceStationData.lat & fromMaybeM (StationError.InvalidStationData "Source station latitude not found")
          sourceLon <- sourceStationData.lon & fromMaybeM (StationError.InvalidStationData "Source station longitude not found")
          destLat <- destStationData.lat & fromMaybeM (StationError.InvalidStationData "Destination station latitude not found")
          destLon <- destStationData.lon & fromMaybeM (StationError.InvalidStationData "Destination station longitude not found")
          let sourceLatLong = LatLong sourceLat sourceLon
              destLatLong = LatLong destLat destLon

          multiModalLeg <- getUpdatedMultiModalLeg sourceLatLong destLatLong sourceStation.stopCode destStation.stopCode journey reqJourneyLeg riderConfig

          return (multiModalLeg, Just sourceStationData, Just destStationData)
        (Just sourceStation, Nothing) -> do
          destStopCode <- reqJourneyLeg.toStopDetails >>= (.stopCode) & fromMaybeM (StationError.InvalidStationData "Current destination station not found")
          sourceStationData <-
            OTPRest.getStationByGtfsIdAndStopCode sourceStation.stopCode integratedBPPConfig
              >>= fromMaybeM (StationError.StationNotFound sourceStation.stopCode)
          sourceLat <- sourceStationData.lat & fromMaybeM (StationError.InvalidStationData "Source station latitude not found")
          sourceLon <- sourceStationData.lon & fromMaybeM (StationError.InvalidStationData "Source station longitude not found")
          let sourceLatLong = LatLong sourceLat sourceLon
              destLatLong = LatLong reqJourneyLeg.endLocation.latitude reqJourneyLeg.endLocation.longitude
          multiModalLeg <- getUpdatedMultiModalLeg sourceLatLong destLatLong sourceStation.stopCode destStopCode journey reqJourneyLeg riderConfig

          return (multiModalLeg, Just sourceStationData, Nothing)
        (Nothing, Just destStation) -> do
          sourceStopCode <- reqJourneyLeg.fromStopDetails >>= (.stopCode) & fromMaybeM (StationError.InvalidStationData "Current source station not found")
          destStationData <-
            OTPRest.getStationByGtfsIdAndStopCode destStation.stopCode integratedBPPConfig
              >>= fromMaybeM (StationError.StationNotFound destStation.stopCode)
          let sourceLatLong = LatLong reqJourneyLeg.startLocation.latitude reqJourneyLeg.startLocation.longitude
          destLat <- destStationData.lat & fromMaybeM (StationError.InvalidStationData "Destination station latitude not found")
          destLon <- destStationData.lon & fromMaybeM (StationError.InvalidStationData "Destination station longitude not found")
          let destLatLong = LatLong destLat destLon
          multiModalLeg <- getUpdatedMultiModalLeg sourceLatLong destLatLong sourceStopCode destStation.stopCode journey reqJourneyLeg riderConfig

          return (multiModalLeg, Nothing, Just destStationData)
        (Nothing, Nothing) ->
          throwError $ InvalidStationChange "" "No stations provided for change"

    getUpdatedMultiModalLeg ::
      (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
      LatLong -> -- Source location
      LatLong -> -- Destination location
      Text -> -- Source stop code
      Text -> -- Destination stop code
      Domain.Types.Journey.Journey ->
      DJourneyLeg.JourneyLeg ->
      DRC.RiderConfig ->
      m MultiModalTypes.MultiModalLeg
    getUpdatedMultiModalLeg sourceLatLong destLatLong sourceStopCode destStopCode journey reqJourneyLeg riderConfig = do
      case reqJourneyLeg.mode of
        DTrip.Metro -> validateAndGetTransitLeg MultiModalTypes.MetroRail sourceLatLong destLatLong sourceStopCode destStopCode journey reqJourneyLeg riderConfig
        DTrip.Subway -> validateAndGetTransitLeg MultiModalTypes.Subway sourceLatLong destLatLong sourceStopCode destStopCode journey reqJourneyLeg riderConfig
        _ -> throwError $ InvalidRequest "Mode not implemented for station changes"

    validateAndGetTransitLeg ::
      (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
      GeneralVehicleType -> -- Transit mode (MetroRail or Subway)
      LatLong -> -- Source location
      LatLong -> -- Destination location
      Text -> -- Source stop code
      Text -> -- Destination stop code
      Domain.Types.Journey.Journey ->
      DJourneyLeg.JourneyLeg ->
      DRC.RiderConfig ->
      m MultiModalTypes.MultiModalLeg
    validateAndGetTransitLeg transitMode sourceLatLong destLatLong sourceStopCode destStopCode journey reqJourneyLeg riderConfig = do
      let transitRoutesReq =
            GetTransitRoutesReq
              { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = sourceLatLong.lat, longitude = sourceLatLong.lon}}},
                destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = destLatLong.lat, longitude = destLatLong.lon}}},
                arrivalTime = Nothing,
                departureTime = Nothing,
                mode = Nothing,
                transitPreferences = Nothing,
                transportModes = Nothing,
                minimumWalkDistance = riderConfig.minimumWalkDistance,
                permissibleModes = fromMaybe [] riderConfig.permissibleModes,
                maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
                sortingType = MultiModal.Fastest,
                walkSpeed = if fromMaybe False journey.isSingleMode then riderConfig.singleModeWalkSpeed else Nothing
              }

      transitServiceReq <- TMultiModal.getTransitServiceReq journey.merchantId reqJourneyLeg.merchantOperatingCityId
      otpResponse <- JMU.measureLatency (MultiModal.getTransitRoutes (Just journeyId.getId) transitServiceReq transitRoutesReq >>= fromMaybeM (OTPServiceUnavailable "No routes found from OTP")) "getTransitRoutes"

      validatedRoute <- JMU.getBestOneWayRoute transitMode otpResponse.routes (Just sourceStopCode) (Just destStopCode) & fromMaybeM (getRouteNotFoundError transitMode sourceStopCode destStopCode)

      transitLeg <- case filter (\leg -> leg.mode == transitMode) validatedRoute.legs of
        [singleTransitLeg] -> return singleTransitLeg
        _ -> throwError $ getLegNotFoundError transitMode "Multiple transit legs found in route"
      return transitLeg

    getRouteNotFoundError :: GeneralVehicleType -> Text -> Text -> MultimodalError
    getRouteNotFoundError MultiModalTypes.MetroRail source dest = NoValidMetroRoute source dest
    getRouteNotFoundError MultiModalTypes.Subway source dest = NoValidSubwayRoute source dest
    getRouteNotFoundError _ source dest = NoValidMetroRoute source dest -- fallback
    getLegNotFoundError :: GeneralVehicleType -> Text -> MultimodalError
    getLegNotFoundError MultiModalTypes.MetroRail reason = MetroLegNotFound reason
    getLegNotFoundError MultiModalTypes.Subway reason = SubwayLegNotFound reason
    getLegNotFoundError _ reason = MetroLegNotFound reason -- fallback
    updateLegsWithGates ::
      (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
      DRC.RiderConfig ->
      Maybe DJourneyLeg.JourneyLeg ->
      Maybe DJourneyLeg.JourneyLeg ->
      DJourneyLeg.JourneyLeg ->
      MultiModalTypes.MultiModalLeg ->
      Id Domain.Types.Merchant.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      Maybe DStation.Station ->
      Maybe DStation.Station ->
      m (Maybe JMTypes.Gates)
    updateLegsWithGates riderConfig mbPrevLeg mbNextLeg oldJourneyLeg multiModalLeg merchantId merchantOpCityId mbSourceStation mbDestStation = do
      mbEntranceGates <- case (mbPrevLeg, mbSourceStation) of
        (Just prevLeg, Just sourceStation) -> do
          let prevLegStartPoint = LatLong prevLeg.startLocation.latitude prevLeg.startLocation.longitude
          (mbOsmEntrance, mbStraightLineEntrance) <- JMTypes.getNearestGateFromLeg prevLegStartPoint merchantId merchantOpCityId (fromMaybe [] sourceStation.gates)
          let bestEntrance = mbOsmEntrance <|> mbStraightLineEntrance
          -- Use new station coordinates when no gates found
          sourceLat <- sourceStation.lat & fromMaybeM (StationError.InvalidStationData "Source station latitude not found")
          sourceLon <- sourceStation.lon & fromMaybeM (StationError.InvalidStationData "Source station longitude not found")
          let newStationLocation = LatLngV2 sourceLat sourceLon
          let prevLegEndLocation = maybe newStationLocation (\e -> LatLngV2 (fromMaybe sourceLat e.lat) (fromMaybe sourceLon e.lon)) bestEntrance
          -- Log when falling back to station coordinates
          when (isNothing bestEntrance) $ do
            logError $ "No gates found for source station " <> sourceStation.code <> ", using station coordinates: " <> show (sourceLat, sourceLon)
          (mbNewDistance, mbNewDuration) <- JMU.getDistanceAndDuration merchantId merchantOpCityId prevLegStartPoint (LatLong prevLegEndLocation.latitude prevLegEndLocation.longitude)
          let (mode, newDistance, newDuration) = updateLegDistanceAndMode prevLeg mbNewDistance mbNewDuration
              updatedPrevLeg =
                prevLeg
                  { DJourneyLeg.endLocation = prevLegEndLocation,
                    DJourneyLeg.toStopDetails = multiModalLeg.fromStopDetails,
                    DJourneyLeg.osmEntrance = mbOsmEntrance,
                    DJourneyLeg.straightLineEntrance = mbStraightLineEntrance,
                    DJourneyLeg.legSearchId = Nothing,
                    DJourneyLeg.distance = newDistance,
                    DJourneyLeg.duration = newDuration,
                    DJourneyLeg.mode = mode
                  }
          QJourneyLeg.updateByPrimaryKey updatedPrevLeg
          return $ Just (mbOsmEntrance, mbStraightLineEntrance)
        _ -> return Nothing

      mbExitGates <- case (mbNextLeg, mbDestStation) of
        (Just nextLeg, Just destStation) -> do
          let nextLegEndPoint = LatLong nextLeg.endLocation.latitude nextLeg.endLocation.longitude
          (mbOsmExit, mbStraightLineExit) <- JMTypes.getNearestGateFromLeg nextLegEndPoint merchantId merchantOpCityId (fromMaybe [] destStation.gates)
          let bestExit = mbOsmExit <|> mbStraightLineExit
          -- Use new station coordinates when no gates found
          destLat <- destStation.lat & fromMaybeM (StationError.InvalidStationData "Destination station latitude not found")
          destLon <- destStation.lon & fromMaybeM (StationError.InvalidStationData "Destination station longitude not found")
          let newStationLocation = LatLngV2 destLat destLon
          let nextLegStartLocation = maybe newStationLocation (\e -> LatLngV2 (fromMaybe destLat e.lat) (fromMaybe destLon e.lon)) bestExit
          -- Log when falling back to station coordinates
          when (isNothing bestExit) $ do
            logError $ "No gates found for destination station " <> destStation.code <> ", using station coordinates: " <> show (destLat, destLon)
          (mbNewDistance, mbNewDuration) <- JMU.getDistanceAndDuration merchantId merchantOpCityId (LatLong nextLegStartLocation.latitude nextLegStartLocation.longitude) nextLegEndPoint
          let (mode, newDistance, newDuration) = updateLegDistanceAndMode nextLeg mbNewDistance mbNewDuration
          let updatedNextLeg =
                nextLeg
                  { DJourneyLeg.startLocation = nextLegStartLocation,
                    DJourneyLeg.fromStopDetails = multiModalLeg.toStopDetails,
                    DJourneyLeg.osmExit = mbOsmExit,
                    DJourneyLeg.straightLineExit = mbStraightLineExit,
                    DJourneyLeg.legSearchId = Nothing,
                    DJourneyLeg.distance = newDistance,
                    DJourneyLeg.duration = newDuration,
                    DJourneyLeg.mode = mode
                  }
          QJourneyLeg.updateByPrimaryKey updatedNextLeg
          return $ Just (mbOsmExit, mbStraightLineExit)
        _ -> return Nothing

      case (mbEntranceGates, mbExitGates) of
        (Nothing, Nothing) -> return Nothing
        _ ->
          let straightLineEntrance = (mbEntranceGates >>= snd) <|> (oldJourneyLeg.straightLineEntrance)
              straightLineExit = (mbExitGates >>= snd) <|> (oldJourneyLeg.straightLineExit)
              osmEntrance = (mbEntranceGates >>= fst) <|> (oldJourneyLeg.osmEntrance)
              osmExit = (mbExitGates >>= fst) <|> (oldJourneyLeg.osmExit)
           in return $ Just JMTypes.Gates {..}
      where
        updateLegDistanceAndMode ::
          DJourneyLeg.JourneyLeg ->
          Maybe Meters ->
          Maybe Seconds ->
          (DTrip.MultimodalTravelMode, Maybe Distance, Maybe Seconds)
        updateLegDistanceAndMode leg mbNewDistance mbNewDuration =
          case mbNewDistance of
            Just newD
              | newD < riderConfig.maximumWalkDistance ->
                ( DTrip.Walk,
                  Just (convertMetersToDistance Meter newD),
                  Just (calculateWalkDuration (convertMetersToDistance Meter newD))
                )
              | leg.mode == DTrip.Walk && newD >= riderConfig.maximumWalkDistance ->
                ( DTrip.Taxi,
                  Just (convertMetersToDistance Meter newD),
                  mbNewDuration
                )
              | otherwise ->
                ( leg.mode,
                  Just (convertMetersToDistance Meter newD),
                  mbNewDuration
                )
            _ -> (leg.mode, leg.distance, leg.duration)

data RouteServiceabilityContext = RouteServiceabilityContext
  { integratedBPPConfig :: DIBC.IntegratedBPPConfig,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    merchantId :: Id Domain.Types.Merchant.Merchant
  }

data ResolvedLeg = ResolvedLeg
  { rlOrder :: Int,
    rlRouteCodes :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

postMultimodalRouteServiceability ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultimodalConfirm.RouteServiceabilityReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.RouteServiceabilityResp
  )
postMultimodalRouteServiceability (mbPersonId, _merchantId) req = do
  person <- authenticate mbPersonId
  integratedBPPConfig <- fromMaybeM (InvalidRequest "Integrated BPP config not found") =<< listToMaybe <$> SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId Enums.BUS DIBC.MULTIMODAL
  let routeServiceabilityContext =
        RouteServiceabilityContext
          { integratedBPPConfig,
            merchantOperatingCityId = person.merchantOperatingCityId,
            merchantId = person.merchantId
          }
  (srcCode, destCode) <- JMU.measureLatency (resolveSrcAndDestCode req.sourceStopCode req.destinationStopCode req.routeCodes routeServiceabilityContext) ("resolveSrcAndDestCode req=" <> show req)
  directRouteCodes <- JMU.measureLatency (JLU.getRouteCodesFromTo srcCode destCode integratedBPPConfig) ("JLU.getRouteCodesFromTo src=" <> srcCode <> " dest=" <> destCode)
  if null directRouteCodes
    then JMU.measureLatency (handleOtpRoute routeServiceabilityContext srcCode destCode) ("handleOtpRoute src=" <> srcCode <> " dest=" <> destCode)
    else JMU.measureLatency (handleDirectRoute routeServiceabilityContext srcCode destCode directRouteCodes) ("handleDirectRoute src=" <> srcCode <> " dest=" <> destCode <> " routeCodes=" <> show directRouteCodes)
  where
    authenticate :: Maybe (Id Domain.Types.Person.Person) -> Environment.Flow Domain.Types.Person.Person
    authenticate mbPersonId' = do
      personId <- mbPersonId' & fromMaybeM (InvalidRequest "Person not found")
      QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

    resolveSrcAndDestCode ::
      Maybe Text ->
      Maybe Text ->
      Maybe [ApiTypes.RouteCodesWithLeg] ->
      RouteServiceabilityContext ->
      Environment.Flow (Text, Text)
    resolveSrcAndDestCode mSrc mDest routeCodes ctx
      | isJust mSrc && isJust mDest =
        pure (fromJust mSrc, fromJust mDest)
      | otherwise = do
        (firstStop, lastStop) <- JMU.measureLatency (fetchRouteBoundaryStops routeCodes ctx) ("fetchRouteBoundaryStops routeCodes=" <> show routeCodes)
        pure (fromMaybe firstStop mSrc, fromMaybe lastStop mDest)

    fetchRouteBoundaryStops ::
      Maybe [ApiTypes.RouteCodesWithLeg] ->
      RouteServiceabilityContext ->
      Environment.Flow (Text, Text)
    fetchRouteBoundaryStops routeCodes ctx = do
      routeCode <- extractRouteCode routeCodes
      stops <-
        OTPRest.getRouteStopMappingByRouteCode
          routeCode
          ctx.integratedBPPConfig
      when (null stops) $
        throwError $
          InvalidRequest ("No stops found for route: " <> routeCode)
      let sorted = sortOn (.sequenceNum) stops
      pure ((.stopCode) (head sorted), (.stopCode) (last sorted))

    extractRouteCode ::
      Maybe [ApiTypes.RouteCodesWithLeg] ->
      Environment.Flow Text
    extractRouteCode =
      maybe
        (throwError $ InvalidRequest "routeCodes required when src/dest missing")
        ( maybe
            (throwError $ InvalidRequest "No routeCodes found in legs")
            pure
            . (listToMaybe >=> listToMaybe . (.routeCodes))
        )

    extractSourceDestLatLng :: Text -> Text -> RouteServiceabilityContext -> Environment.Flow (LatLngV2, LatLngV2)
    extractSourceDestLatLng srcCode destCode routeServiceabilityContext = do
      sourceStationData <-
        OTPRest.getStationByGtfsIdAndStopCode srcCode routeServiceabilityContext.integratedBPPConfig
          >>= fromMaybeM (StationError.StationNotFound srcCode)
      destStationData <-
        OTPRest.getStationByGtfsIdAndStopCode destCode routeServiceabilityContext.integratedBPPConfig
          >>= fromMaybeM (StationError.StationNotFound destCode)
      sourceLat <- sourceStationData.lat & fromMaybeM (StationError.InvalidStationData "Source station latitude not found")
      sourceLon <- sourceStationData.lon & fromMaybeM (StationError.InvalidStationData "Source station longitude not found")
      destLat <- destStationData.lat & fromMaybeM (StationError.InvalidStationData "Destination station latitude not found")
      destLon <- destStationData.lon & fromMaybeM (StationError.InvalidStationData "Destination station longitude not found")
      pure (LatLngV2 sourceLat sourceLon, LatLngV2 destLat destLon)

    buildRouteWithLiveVehicle ::
      CQMMB.RouteWithBuses ->
      RouteServiceabilityContext ->
      Bool ->
      Environment.Flow (Maybe API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle)
    buildRouteWithLiveVehicle routeInfo routeServiceabilityContext enableDebug = do
      route <-
        OTPRest.getRouteByRouteId routeServiceabilityContext.integratedBPPConfig routeInfo.routeId
          >>= fromMaybeM
            (InvalidRequest $ "Route not found with id: " <> routeInfo.routeId)
      busScheduleDetails <-
        OTPRest.getRouteBusSchedule routeInfo.routeId routeServiceabilityContext.integratedBPPConfig
      schedules <-
        getBusScheduleInfo busScheduleDetails routeServiceabilityContext
      when enableDebug $
        logDebug $
          "AlternateRoute getRouteBusSchedule routeId="
            <> routeInfo.routeId
            <> ", scheduleCount="
            <> show (length schedules)
      liveVehicles <- getLiveVehicles routeInfo.buses routeServiceabilityContext
      when enableDebug $
        logDebug $
          "AlternateRoute routeId="
            <> routeInfo.routeId
            <> ", shortName="
            <> route.shortName
      if null liveVehicles && null schedules
        then pure Nothing
        else
          pure $
            Just
              API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle
                { liveVehicles,
                  schedules,
                  routeCode = routeInfo.routeId,
                  routeShortName = route.shortName
                }

    getValidSingleModeRoute :: RouteServiceabilityContext -> Text -> Text -> Environment.Flow MultiModalTypes.MultiModalRoute
    getValidSingleModeRoute routeServiceabilityContext srcCode' destCode' = do
      riderConfig <- QRiderConfig.findByMerchantOperatingCityId routeServiceabilityContext.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound routeServiceabilityContext.merchantOperatingCityId.getId)
      (sourceLatLong', destLatLong') <- extractSourceDestLatLng srcCode' destCode' routeServiceabilityContext
      let transitRoutesReq =
            MultiModalTypes.GetTransitRoutesReq
              { origin = WayPointV2 {location = LocationV2 {latLng = sourceLatLong'}},
                destination = WayPointV2 {location = LocationV2 {latLng = destLatLong'}},
                arrivalTime = Nothing,
                departureTime = Nothing,
                mode = Nothing,
                transitPreferences = Nothing,
                transportModes = Nothing,
                minimumWalkDistance = riderConfig.minimumWalkDistance,
                permissibleModes = fromMaybe [] riderConfig.permissibleModes,
                maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
                sortingType = MultiModal.Fastest,
                walkSpeed = Nothing
              }
      transitServiceReq <- TMultiModal.getTransitServiceReq routeServiceabilityContext.merchantId routeServiceabilityContext.merchantOperatingCityId
      otpResponse <- JMU.measureLatency (MultiModal.getTransitRoutes Nothing transitServiceReq transitRoutesReq >>= fromMaybeM (OTPServiceUnavailable "No routes found from OTP")) ("MultiModal.getTransitRoutes req=" <> show transitRoutesReq)
      oneWayRouteWithWalkLegs <- JMU.getBestOneWayRoute MultiModalTypes.Bus otpResponse.routes (Just srcCode') (Just destCode') & fromMaybeM (getRouteNotFoundError MultiModalTypes.Bus srcCode' destCode')
      pure $ onlyBusLegs oneWayRouteWithWalkLegs

    onlyBusLegs :: MultiModalTypes.MultiModalRoute -> MultiModalTypes.MultiModalRoute
    onlyBusLegs route =
      route {legs = filter (\l -> l.mode == MultiModal.Bus) route.legs}

    getStopGtfsCode :: Maybe MultiModalStopDetails -> Maybe Text
    getStopGtfsCode =
      fmap Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode . (>>= (.gtfsId))

    getLegRouteCodes :: MultiModalTypes.MultiModalLeg -> DIBC.IntegratedBPPConfig -> Environment.Flow [Text]
    getLegRouteCodes leg integratedBPPConfig = do
      let routeDetails = leg.routeDetails

      when (length routeDetails > 1) $
        logWarning $
          "Bus leg has more than one routeDetail. count="
            <> show (length routeDetails)
            <> ", routeIds="
            <> show (map (.gtfsId) routeDetails)
            <> ", leg="
            <> show leg

      -- Only first routeDetail to be considered since this is bus
      let routeCodeFromLeg =
            maybeToList $
              listToMaybe routeDetails
                >>= \rd ->
                  Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode <$> rd.gtfsId

      let mSrcCode = getStopGtfsCode leg.fromStopDetails
          mDestCode = getStopGtfsCode leg.toStopDetails

      stopToStopRouteCodes <-
        case (mSrcCode, mDestCode) of
          (Just src, Just dest) ->
            JLU.getRouteCodesFromTo src dest integratedBPPConfig
          _ ->
            pure []

      pure $ nub (routeCodeFromLeg <> stopToStopRouteCodes)

    getBusScheduleInfo :: NandiTypes.BusScheduleDetails -> RouteServiceabilityContext -> Environment.Flow [API.Types.UI.MultimodalConfirm.ScheduledVehicleInfo]
    getBusScheduleInfo busScheduleDetails routeServiceabilityContext =
      catMaybes
        <$> mapM
          ( \detail -> do
              busLiveInfo <- JLCF.getBusLiveInfo detail.vehicle_no routeServiceabilityContext.integratedBPPConfig
              logDebug $ "getBusScheduleInfo: getBusLiveInfo vehicle=" <> detail.vehicle_no <> ", found=" <> show (isJust busLiveInfo) <> ", details=" <> show (fmap (\v -> (v.vehicle_number, v.latitude, v.longitude, v.timestamp, v.routes_info, v.bearing)) busLiveInfo)
              mbServiceTier <- JMU.getVehicleServiceTypeFromInMem [routeServiceabilityContext.integratedBPPConfig] detail.vehicle_no
              case mbServiceTier of
                Just serviceTier -> do
                  frfsServiceTier <- CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier routeServiceabilityContext.merchantOperatingCityId routeServiceabilityContext.integratedBPPConfig.id
                  -- Get service subtypes from in-memory cache
                  mbServiceSubTypes <- JMU.getVehicleServiceSubTypesFromInMem [routeServiceabilityContext.integratedBPPConfig] detail.vehicle_no
                  logDebug $ "getBusScheduleInfo: vehicle=" <> detail.vehicle_no <> ", serviceTier=" <> show serviceTier <> ", frfsName=" <> show ((.shortName) <$> frfsServiceTier) <> ", hasLiveInfo=" <> show (isJust busLiveInfo) <> ", eta=" <> show detail.eta <> ", position=" <> show ((\bli -> LatLong bli.latitude bli.longitude) <$> busLiveInfo) <> ", timestamp=" <> show ((.timestamp) <$> busLiveInfo)
                  return . Just $
                    API.Types.UI.MultimodalConfirm.ScheduledVehicleInfo
                      { eta = Just detail.eta,
                        position = (\bli -> LatLong bli.latitude bli.longitude) <$> busLiveInfo,
                        locationUTCTimestamp = posixSecondsToUTCTime . fromIntegral . (.timestamp) <$> busLiveInfo,
                        serviceTierType = serviceTier,
                        serviceTierName = (.shortName) <$> frfsServiceTier,
                        vehicleNumber = detail.vehicle_no,
                        serviceSubTypes = mbServiceSubTypes
                      }
                Nothing -> do
                  logError $ "Vehicle info not found for bus: " <> detail.vehicle_no
                  return Nothing
          )
          busScheduleDetails

    getLiveVehicles :: [CQMMB.FullBusData] -> RouteServiceabilityContext -> Environment.Flow [API.Types.UI.MultimodalConfirm.LiveVehicleInfo]
    getLiveVehicles busesData routeServiceabilityContext =
      catMaybes
        <$> mapM
          ( \bus -> do
              mbServiceTier <- JLU.getVehicleServiceTypeFromInMem [routeServiceabilityContext.integratedBPPConfig] bus.vehicleNumber
              case mbServiceTier of
                Just serviceTier -> do
                  frfsServiceTier <- CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier routeServiceabilityContext.merchantOperatingCityId routeServiceabilityContext.integratedBPPConfig.id
                  -- Get service subtypes from in-memory cache
                  mbServiceSubTypes <- JMU.getVehicleServiceSubTypesFromInMem [routeServiceabilityContext.integratedBPPConfig] bus.vehicleNumber
                  logDebug $ "getLiveVehicles: vehicle=" <> bus.vehicleNumber <> ", routeId=" <> bus.busData.route_id <> ", serviceTier=" <> show serviceTier <> ", frfsName=" <> show ((.shortName) <$> frfsServiceTier) <> ", position=(" <> show bus.busData.latitude <> "," <> show bus.busData.longitude <> ")" <> ", timestamp=" <> show bus.busData.timestamp <> ", eta=" <> show bus.busData.eta_data <> ", routeState=" <> show bus.busData.route_state <> ", routeNumber=" <> show bus.busData.route_number
                  enrichedEta <-
                    mapConcurrently
                      (enrichBusStopETA routeServiceabilityContext.integratedBPPConfig)
                      (fromMaybe [] bus.busData.eta_data)
                  return . Just $
                    API.Types.UI.MultimodalConfirm.LiveVehicleInfo
                      { eta = Just enrichedEta,
                        number = bus.vehicleNumber,
                        position = LatLong bus.busData.latitude bus.busData.longitude,
                        locationUTCTimestamp = posixSecondsToUTCTime $ fromIntegral bus.busData.timestamp,
                        serviceTierType = serviceTier,
                        serviceTierName = (.shortName) <$> frfsServiceTier,
                        serviceSubTypes = mbServiceSubTypes
                      }
                Nothing -> do
                  logError $ "Vehicle info not found for bus: " <> bus.vehicleNumber
                  return Nothing
          )
          busesData

    enrichBusStopETA :: DIBC.IntegratedBPPConfig -> CQMMB.BusStopETA -> Environment.Flow CQMMB.BusStopETA
    enrichBusStopETA integratedBPPConfig' eta =
      case eta.stopName of
        Just _ ->
          pure eta
        Nothing -> do
          mbStation <-
            OTPRest.getStationByGtfsIdAndStopCode
              eta.stopCode
              integratedBPPConfig'
          let fetchedName = fmap (.name) mbStation
          pure $
            eta{CQMMB.stopName = fetchedName <|> eta.stopName}

    getRouteNotFoundError :: GeneralVehicleType -> Text -> Text -> MultimodalError
    getRouteNotFoundError MultiModalTypes.MetroRail source dest = NoValidMetroRoute source dest
    getRouteNotFoundError MultiModalTypes.Subway source dest = NoValidSubwayRoute source dest
    getRouteNotFoundError MultiModalTypes.Bus source dest = NoValidBusRoute source dest
    getRouteNotFoundError _ source dest = NoValidMetroRoute source dest -- fallback
    getRouteServiceability ::
      Maybe ApiTypes.EffectiveStops ->
      RouteServiceabilityContext ->
      [ResolvedLeg] ->
      Environment.Flow API.Types.UI.MultimodalConfirm.RouteServiceabilityResp
    getRouteServiceability mEffStops ctx legs = do
      legRoutes <- JMU.measureLatency (enrichResolvedLegs ctx legs) ("enrichResolvedLegs legsCount=" <> show (length legs))
      pure $ ApiTypes.RouteServiceabilityResp mEffStops legRoutes

    handleDirectRoute ::
      RouteServiceabilityContext ->
      Text ->
      Text ->
      [Text] ->
      Environment.Flow API.Types.UI.MultimodalConfirm.RouteServiceabilityResp
    handleDirectRoute ctx srcCode destCode directRouteCodes = do
      let resolvedLegs =
            resolveLegsForDirectRoute srcCode destCode directRouteCodes
      JMU.measureLatency (getRouteServiceability Nothing ctx resolvedLegs) ("getRouteServiceability legsCount=" <> show (length resolvedLegs))

    handleOtpRoute ::
      RouteServiceabilityContext ->
      Text ->
      Text ->
      Environment.Flow API.Types.UI.MultimodalConfirm.RouteServiceabilityResp
    handleOtpRoute ctx srcCode destCode = do
      (effectiveStops, resolvedLegs) <-
        JMU.measureLatency (resolveLegsViaOtpCached ctx srcCode destCode) ("resolveLegsViaOtpCached src=" <> srcCode <> " dest=" <> destCode)

      JMU.measureLatency (getRouteServiceability effectiveStops ctx resolvedLegs) ("getRouteServiceability legsCount=" <> show (length resolvedLegs))

    makeOtpResolvedRouteKey ::
      RouteServiceabilityContext ->
      Text -> -- src stop
      Text -> -- dest stop
      Text
    makeOtpResolvedRouteKey ctx srcCode destCode =
      "otp-resolved-route-"
        <> show ctx.merchantOperatingCityId
        <> "-"
        <> normalize srcCode
        <> "-"
        <> normalize destCode
        <> "-"
        <> show ctx.integratedBPPConfig.id.getId
      where
        normalize = T.toUpper . T.strip

    resolveLegsViaOtpCached ::
      RouteServiceabilityContext ->
      Text ->
      Text ->
      Environment.Flow (Maybe ApiTypes.EffectiveStops, [ResolvedLeg])
    resolveLegsViaOtpCached ctx srcCode destCode = do
      let key = makeOtpResolvedRouteKey ctx srcCode destCode
      cached <- Hedis.safeGet key
      case cached of
        Just result ->
          pure result
        Nothing -> do
          result@(_, legs) <- JMU.measureLatency (resolveLegsViaOtp srcCode destCode ctx) ("resolveLegsViaOtp src=" <> srcCode <> " dest=" <> destCode)
          -- IMPORTANT: only cache non-empty successful results
          unless (null legs) $
            Hedis.setExp key result 86400
          pure result

    enrichResolvedLegs ::
      RouteServiceabilityContext ->
      [ResolvedLeg] ->
      Environment.Flow [ApiTypes.LegRouteWithLiveVehicle]
    enrichResolvedLegs ctx =
      mapConcurrently enrichLeg
      where
        enrichLeg ResolvedLeg {..} = do
          busesForRoutes <-
            CQMMB.getBusesForRoutes rlRouteCodes ctx.integratedBPPConfig

          routesWithLiveVehicles <-
            catMaybes
              <$> mapM
                (\r -> buildRouteWithLiveVehicle r ctx False)
                busesForRoutes

          pure $
            ApiTypes.LegRouteWithLiveVehicle
              { legOrder = rlOrder,
                routeWithLiveVehicles = routesWithLiveVehicles
              }

    resolveLegsViaOtp ::
      Text ->
      Text ->
      RouteServiceabilityContext ->
      Environment.Flow (Maybe ApiTypes.EffectiveStops, [ResolvedLeg])
    resolveLegsViaOtp srcCode destCode ctx = do
      validatedRoute <- JMU.measureLatency (getValidSingleModeRoute ctx srcCode destCode) ("getValidSingleModeRoute src=" <> srcCode <> " dest=" <> destCode)
      case validatedRoute.legs of
        [] -> pure (Nothing, [])
        legs -> do
          let firstLeg = head legs
              lastLeg = last legs
          (effSrc, effDest) <-
            fromMaybeM
              (InvalidRequest "OTP returned legs without stop codes")
              ( (,) <$> getStopGtfsCode firstLeg.fromStopDetails
                  <*> getStopGtfsCode lastLeg.toStopDetails
              )
          resolvedLegs <-
            mapM
              ( \(idx, leg) -> do
                  codes <- getLegRouteCodes leg ctx.integratedBPPConfig
                  pure $
                    ResolvedLeg
                      { rlOrder = idx,
                        rlRouteCodes = codes
                      }
              )
              (zip [0 ..] legs)
          effectiveStops <- JMU.measureLatency (calculateEffectiveStops srcCode destCode effSrc effDest ctx.integratedBPPConfig) ("calculateEffectiveStops src=" <> srcCode <> " dest=" <> destCode <> " effSrc=" <> effSrc <> " effDest=" <> effDest)
          pure
            (Just effectiveStops, resolvedLegs)

    sameStation :: DStation.Station -> DStation.Station -> Bool
    sameStation a b =
      normalize (a.name) == normalize (b.name)
      where
        normalize = T.toCaseFold . T.strip

    sameStationMaybe :: Maybe DStation.Station -> Maybe DStation.Station -> Bool
    sameStationMaybe (Just a) (Just b) = sameStation a b
    sameStationMaybe Nothing Nothing = False
    sameStationMaybe _ _ = False

    calculateEffectiveStops ::
      Text ->
      Text ->
      Text ->
      Text ->
      DIBC.IntegratedBPPConfig ->
      Environment.Flow ApiTypes.EffectiveStops
    calculateEffectiveStops srcCode destCode snappedSrcCode snappedDestCode integratedBPPConfig' = do
      srcStop <-
        OTPRest.getStationByGtfsIdAndStopCode srcCode integratedBPPConfig'
      destStop <-
        OTPRest.getStationByGtfsIdAndStopCode destCode integratedBPPConfig'
      snappedSrcStop <-
        OTPRest.getStationByGtfsIdAndStopCode snappedSrcCode integratedBPPConfig'
      snappedDestStop <-
        OTPRest.getStationByGtfsIdAndStopCode snappedDestCode integratedBPPConfig'
      let sourceChanged = not (sameStationMaybe srcStop snappedSrcStop)
      let destinationChanged = not (sameStationMaybe destStop snappedDestStop)
      pure
        ApiTypes.EffectiveStops
          { requestedSourceStop = srcCode,
            requestedDestinationStop = destCode,
            effectiveSourceStop = snappedSrcCode,
            effectiveDestinationStop = snappedDestCode,
            sourceStopNameChanged = sourceChanged,
            destStopNameChanged = destinationChanged
          }

    resolveLegsForDirectRoute ::
      Text ->
      Text ->
      [Text] ->
      [ResolvedLeg]
    resolveLegsForDirectRoute _src _dest directRouteCodes =
      [ ResolvedLeg
          { rlOrder = 0,
            rlRouteCodes = directRouteCodes
          }
      ]

postMultimodalRouteAvailability ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultimodalConfirm.RouteAvailabilityReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.RouteAvailabilityResp
  )
postMultimodalRouteAvailability (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Get current time for route search
  currentTime <- getCurrentTime

  -- Determine vehicle category based on the route search (defaulting to Bus for general routes)
  let vehicleCategory = Enums.BUS

  -- Find integrated BPP configs for the merchant operating city
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId vehicleCategory DIBC.MULTIMODAL

  -- Get rider config to check source of service tier
  mbSourceOfServiceTier <- fmap (.sourceOfServiceTier) <$> QRiderConfig.findByMerchantOperatingCityId person.merchantOperatingCityId

  frfsQuotesAndCategories <-
    case (req.journeyId, req.legOrder) of
      (Just journeyId, Just legOrder) -> do
        journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
        quotes <- maybe (pure []) (QFRFSQuote.findAllBySearchId . Id) journeyLeg.legSearchId
        mapM
          ( \quote -> do
              quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
              return (quote, quoteCategories)
          )
          quotes
      _ -> pure []
  let frfsQuotes = fst <$> frfsQuotesAndCategories

  let availableServiceTiers = if null frfsQuotes then Nothing else Just (mapMaybe (\(quote, quoteCategories) -> JMU.getServiceTierFromQuote quoteCategories quote) frfsQuotesAndCategories)
  case integratedBPPConfigs of
    [] -> return $ ApiTypes.RouteAvailabilityResp {availableRoutes = []}
    (integratedBPPConfig : _) -> do
      (_, availableRoutesByTier, _) <-
        JMU.findPossibleRoutes
          availableServiceTiers
          req.startStopCode
          req.endStopCode
          currentTime
          integratedBPPConfig
          merchantId
          person.merchantOperatingCityId
          vehicleCategory
          True -- sendWithoutFare
          True
          False
          False
      let UTCTime _date secondsDiffTimeTillNow = CQMMB.utcToIST currentTime
      let secondsTillNow = fromIntegral $ div (diffTimeToPicoseconds secondsDiffTimeTillNow) 1000000000000
      let filteredRoutesByTiming = map (\routeByTier -> routeByTier {RD.availableRoutesInfo = filter (\route -> not . null $ filter (\timing -> timing - secondsTillNow > 0 && timing - secondsTillNow < 7200) route.routeTimings) routeByTier.availableRoutesInfo}) availableRoutesByTier
      let (liveBuses, staticBuses) = partition (\route -> route.source == LIVE) filteredRoutesByTiming
      let filteredRoutes =
            if req.onlyLive
              then liveBuses
              else liveBuses <> staticBuses
      case mbSourceOfServiceTier of
        Just DRC.QUOTES -> do
          availableRoutes <- concatMapM (convertToAvailableRouteWithQuotes integratedBPPConfig person frfsQuotes) filteredRoutes
          return $ ApiTypes.RouteAvailabilityResp {availableRoutes = availableRoutes}
        _ -> do
          availableRoutes <- concatMapM (convertToAvailableRoute integratedBPPConfig person) filteredRoutes
          return $ ApiTypes.RouteAvailabilityResp {availableRoutes = availableRoutes}
  where
    findQuoteIdByRouteCode :: [DFRFSQuote.FRFSQuote] -> [Text] -> Maybe (Id DFRFSQuote.FRFSQuote)
    findQuoteIdByRouteCode quotes routeCodes =
      listToMaybe $
        catMaybes $
          map
            ( \quote -> do
                routeStations <- decodeFromText =<< quote.routeStationsJson :: Maybe [FRFSTicketServiceAPI.FRFSRouteStationsAPI]
                let quoteRouteCodes = map (.code) routeStations
                if any (`elem` quoteRouteCodes) routeCodes
                  then Just quote.id
                  else Nothing
            )
            quotes

    convertToAvailableRoute :: DIBC.IntegratedBPPConfig -> Domain.Types.Person.Person -> RD.AvailableRoutesByTier -> Environment.Flow [ApiTypes.AvailableRoute]
    convertToAvailableRoute integratedBPPConfig person routesByTier =
      mapM
        ( \routeInfo -> do
            serviceTierName <-
              case routesByTier.serviceTierName of
                Just _ -> return routesByTier.serviceTierName
                Nothing -> do
                  frfsServiceTier <- CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId routesByTier.serviceTier person.merchantOperatingCityId integratedBPPConfig.id
                  return $ frfsServiceTier <&> (.shortName)
            return $
              ApiTypes.AvailableRoute
                { source = routeInfo.source,
                  quoteId = routesByTier.quoteId,
                  serviceTierName,
                  serviceTierType = routesByTier.serviceTier,
                  routeCode = routeInfo.routeCode,
                  routeShortName = routeInfo.shortName,
                  routeLongName = routeInfo.longName,
                  routeTimings = routeInfo.routeTimings
                }
        )
        routesByTier.availableRoutesInfo

    convertToAvailableRouteWithQuotes :: DIBC.IntegratedBPPConfig -> Domain.Types.Person.Person -> [DFRFSQuote.FRFSQuote] -> RD.AvailableRoutesByTier -> Environment.Flow [ApiTypes.AvailableRoute]
    convertToAvailableRouteWithQuotes integratedBPPConfig person quotes routesByTier =
      mapMaybeM
        ( \routeInfo -> do
            serviceTierName <-
              case routesByTier.serviceTierName of
                Just _ -> return routesByTier.serviceTierName
                Nothing -> do
                  frfsServiceTier <- CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId routesByTier.serviceTier person.merchantOperatingCityId integratedBPPConfig.id
                  return $ frfsServiceTier <&> (.shortName)
            -- Find quoteId for this specific route by matching routeCode
            let mbQuoteId = findQuoteIdByRouteCode quotes [routeInfo.routeCode]
            case mbQuoteId of
              Just quoteId ->
                return $
                  Just $
                    ApiTypes.AvailableRoute
                      { source = routeInfo.source,
                        quoteId = Just quoteId,
                        serviceTierName,
                        serviceTierType = routesByTier.serviceTier,
                        routeCode = routeInfo.routeCode,
                        routeShortName = routeInfo.shortName,
                        routeLongName = routeInfo.longName,
                        routeTimings = routeInfo.routeTimings
                      }
              Nothing -> return Nothing
        )
        routesByTier.availableRoutesInfo

postMultimodalSwitchRoute ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultimodalConfirm.SwitchRouteReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalSwitchRoute (mbPersonId, merchantId) req = do
  journeyLeg <- QJourneyLeg.getJourneyLeg req.journeyId req.legOrder
  QRouteDetails.updateRoute (Just req.routeCode) (Just req.routeCode) (Just req.routeLongName) (Just req.routeShortName) journeyLeg.id.getId
  postMultimodalOrderSwitchFRFSTier (mbPersonId, merchantId) req.journeyId req.legOrder (API.Types.UI.MultimodalConfirm.SwitchFRFSTierReq req.quoteId)

postMultimodalOrderSublegSetOnboardedVehicleDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.OnboardedVehicleDetailsReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSublegSetOnboardedVehicleDetails (_mbPersonId, _merchantId) journeyId legOrder _subLegOrder req = do
  let vehicleNumber = req.vehicleNumber
  mbVehicleOverrideInfo <- Dispatcher.getFleetOverrideInfo vehicleNumber
  journey <- JM.getJourney journeyId
  journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
  legSearchId <- journeyLeg.legSearchId & fromMaybeM (InvalidRequest $ "Leg search ID not found for journey: " <> journeyLeg.id.getId)
  booking <- QFRFSTicketBooking.findBySearchId (Id legSearchId) >>= fromMaybeM (BookingNotFound $ "FRFS booking with search ID:" <> legSearchId)
  quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound $ "FRFS quote with ID:" <> booking.quoteId.getId)
  vehicleType <-
    case journeyLeg.mode of
      DTrip.Bus -> return Enums.BUS
      DTrip.Metro -> return Enums.METRO
      DTrip.Subway -> return Enums.SUBWAY
      _ -> throwError $ UnsupportedVehicleType (show journeyLeg.mode)
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig journey.merchantOperatingCityId vehicleType DIBC.MULTIMODAL
  (integratedBPPConfig, vehicleLiveRouteInfo) <- case mbVehicleOverrideInfo of
    Just (overridenVehicleNumber, waybillNo) -> do
      vehicleLiveRouteInfo <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs overridenVehicleNumber Nothing >>= fromMaybeM (VehicleUnserviceableOnRoute "Vehicle not found on any route")
      if Just waybillNo /= ((.waybillId) . snd $ vehicleLiveRouteInfo)
        then do
          Dispatcher.delFleetOverrideInfo vehicleNumber
          JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber Nothing >>= fromMaybeM (VehicleUnserviceableOnRoute "Vehicle not found on any route")
        else pure vehicleLiveRouteInfo
    Nothing -> JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber Nothing >>= fromMaybeM (VehicleUnserviceableOnRoute "Vehicle not found on any route")
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId journey.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound journey.merchantOperatingCityId.getId)
  -- looks like this as of now 😢, should have one element -> [{"code":"3880","color":null,"endPoint":{"lat":13.05177,"lon":80.09496},"longName":"REDHILLS BUS TERMINUS To POONAMALLEE B.T","priceWithCurrency":{"amount":25,"currency":"INR"},"sequenceNum":null,"shortName":"62","startPoint":{"lat":13.19305,"lon":80.18437},"stations":[{"address":null,"code":"tJbXQuOE","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.13037,"lon":80.15852,"name":"PUDUR","routeCodes":null,"sequenceNum":17,"stationType":"START","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"CpULuhdq","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12724,"lon":80.1539,"name":"AMBATTUR ORAGADAM","routeCodes":null,"sequenceNum":18,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"wFSILCzc","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12182,"lon":80.14794,"name":"RAAKI THEATRE","routeCodes":null,"sequenceNum":19,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"FlYjxOxQ","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12446,"lon":80.14361,"name":"THIRUMULLAIVOYAL STEDFORD HOSPITAL","routeCodes":null,"sequenceNum":20,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"GkYZbgqg","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.1281,"lon":80.13972,"name":"THIRUMULLAIVOYAL SARASWATHI NAGAR CTH SALAI","routeCodes":null,"sequenceNum":21,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"FStgRiBS","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.13049,"lon":80.13634,"name":"THIRUMULLAIVOYAL MANIKANDAPURAM","routeCodes":null,"sequenceNum":22,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"XiLvnZTk","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.13073,"lon":80.13103,"name":"THIRUMULLAIVOYAL","routeCodes":null,"sequenceNum":23,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"Vlipnutt","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12964,"lon":80.12503,"name":"THIRUMULLAIVOYAL VAISHNAVI NAGAR","routeCodes":null,"sequenceNum":24,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"bHagonKv","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.1258,"lon":80.11913,"name":"MURUGAPPA POLYTECHNIC","routeCodes":null,"sequenceNum":25,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"PhjcDdmq","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12338,"lon":80.1125,"name":"AVADI TUBE PRODUCTS OF INDIA","routeCodes":null,"sequenceNum":26,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"vacPrbQA","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11972,"lon":80.10169,"name":"AVADI BUS TERMINUS","routeCodes":null,"sequenceNum":27,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"SGnJOquj","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11942,"lon":80.096,"name":"AVADI CHECK POST","routeCodes":null,"sequenceNum":28,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"pGHnvifU","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11704,"lon":80.09786,"name":"AVADI GOVERNMENT HOSPITAL","routeCodes":null,"sequenceNum":29,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"qNiTyrkl","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11658,"lon":80.10061,"name":"KANNIGAPURAM AVADI","routeCodes":null,"sequenceNum":30,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"FldHcjZC","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11575,"lon":80.10516,"name":"AVADI MARKET","routeCodes":null,"sequenceNum":31,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"9d18d389df1961e2f3d3c19e315bf99a","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11158,"lon":80.10868,"name":"AVADI JB ESTATE","routeCodes":null,"sequenceNum":32,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"f8687de5e5e79549349ff84b70dda40c","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.10552,"lon":80.10853,"name":"AVADI VASANTHAM NAGAR","routeCodes":null,"sequenceNum":33,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"ae4f951daf0a29a3e562512b767b0a24","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.10265,"lon":80.10843,"name":"AVADI MOORTHY NAGAR","routeCodes":null,"sequenceNum":34,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"BJDTCJYP","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.09841,"lon":80.10839,"name":"GOVARDANAGIRI","routeCodes":null,"sequenceNum":35,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"VMzKgUSR","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.09361,"lon":80.1085,"name":"IYANKULAM","routeCodes":null,"sequenceNum":36,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"GJEXlFdt","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.08655,"lon":80.10869,"name":"PARUTHIPATTU ROAD JUNCTION","routeCodes":null,"sequenceNum":37,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"dgLtpUgu","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.08167,"lon":80.10932,"name":"MELPAKKAM","routeCodes":null,"sequenceNum":38,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"NRaiNGwl","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.07822,"lon":80.11062,"name":"KADUVETTI","routeCodes":null,"sequenceNum":39,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"qLQHuyIe","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.06628,"lon":80.11255,"name":"SA ENGINEERING COLLEGE","routeCodes":null,"sequenceNum":40,"stationType":"END","timeTakenToTravelUpcomingStop":null,"towards":null}],"travelTime":null,"vehicleServiceTier":{"_type":"EXECUTIVE","description":"DELUX BUSES","isAirConditioned":false,"longName":"DELUX BUSES","providerCode":"S","shortName":"DELUX BUSES"}}]
  let routeStations :: Maybe [FRFSTicketService.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
  let mbServiceTier = listToMaybe $ mapMaybe (.vehicleServiceTier) (fromMaybe [] routeStations)
  case mbServiceTier of
    Just serviceTier -> do
      let allowedVariants = maybe (Utils.defaultBusBoardingRelationshitCfg serviceTier._type) (.canBoardIn) $ find (\serviceRelationShip -> serviceRelationShip.vehicleType == Enums.BUS && serviceRelationShip.serviceTierType == serviceTier._type) =<< riderConfig.serviceTierRelationshipCfg
      unless (vehicleLiveRouteInfo.serviceType `elem` allowedVariants) $
        throwError $ VehicleServiceTierUnserviceable ("Vehicle " <> vehicleLiveRouteInfo.vehicleNumber <> ", the service tier" <> show vehicleLiveRouteInfo.serviceType <> ", not found on any route: " <> show allowedVariants)
    Nothing -> do
      -- todo: MERTRICS add metric here
      logError $ "CRITICAL: Service tier not found for vehicle, skipping validation " <> vehicleLiveRouteInfo.vehicleNumber
  let journeyLegRouteCodes = nub (mapMaybe (.routeCode) journeyLeg.routeDetails <> (concat $ mapMaybe (.alternateRouteIds) journeyLeg.routeDetails))

  case vehicleLiveRouteInfo.routeCode of
    Just routeCode -> do
      unless (routeCode `elem` journeyLegRouteCodes) $ do
        logError $ "Vehicle " <> vehicleLiveRouteInfo.vehicleNumber <> ", the route code " <> routeCode <> ", not found on any route: " <> show journeyLegRouteCodes <> ", Please board the bus moving on allowed possible Routes for the booking."
        when (riderConfig.validateSetOnboardingVehicleRequest == Just True) $
          throwError $ VehicleUnserviceableOnRoute ("Vehicle " <> vehicleLiveRouteInfo.vehicleNumber <> ", the route code " <> routeCode <> ", not found on any route: " <> show journeyLegRouteCodes <> ", Please board the bus moving on allowed possible Routes for the booking.")
    Nothing -> logError $ "Vehicle " <> vehicleLiveRouteInfo.vehicleNumber <> " not found on any route " <> show journeyLegRouteCodes <> ", Please board the bus moving on allowed possible Routes for the booking."

  let mbNewRouteCode = (vehicleLiveRouteInfo.routeCode,) <$> (listToMaybe journeyLeg.routeDetails) -- doing list to maybe as onluy need from and to stop codes, which will be same in all tickets
  updateTicketQRData journey journeyLeg riderConfig integratedBPPConfig booking.id mbNewRouteCode vehicleLiveRouteInfo
  QJourneyLeg.updateByPrimaryKey $
    journeyLeg
      { DJourneyLeg.finalBoardedBusNumber = Just vehicleLiveRouteInfo.vehicleNumber,
        DJourneyLeg.finalBoardedBusNumberSource = Just DJourneyLeg.UserActivated,
        DJourneyLeg.finalBoardedDepotNo = vehicleLiveRouteInfo.depot,
        DJourneyLeg.finalBoardedWaybillId = vehicleLiveRouteInfo.waybillId,
        DJourneyLeg.finalBoardedScheduleNo = vehicleLiveRouteInfo.scheduleNo,
        DJourneyLeg.finalBoardedBusServiceTierType = Just vehicleLiveRouteInfo.serviceType
      }
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
  where
    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" utcTime

    parseUtcTime :: Text -> Maybe UTCTime
    parseUtcTime t = parseTimeM True defaultTimeLocale "%d-%m-%Y %H:%M:%S" (T.unpack t)

    updateTicketQRData journey journeyLeg riderConfig integratedBPPConfig ticketBookingId mbNewRouteCode vehicleLiveRouteInfo = do
      now <- getCurrentTime
      let newExpiryTimeIst = addUTCTime (fromIntegral $ fromMaybe 7200 riderConfig.updateTicketValidityInSecondsPostSetOnboarding) (addUTCTime (secondsToNominalDiffTime 19800) now)
      updatedTickets <-
        DirectExternalBPP.generateUpdatedQRTicket
          integratedBPPConfig
          ticketBookingId
          ( \ticket -> do
              let mbPrevExpiryTime = parseUtcTime ticket.expiry
              let newTicket =
                    case mbPrevExpiryTime of
                      Just prevExpiryTime -> do
                        let finalNewExpiryIst = min newExpiryTimeIst prevExpiryTime
                        ticket {ExternalBPP.ExternalAPI.Types.expiry = formatUtcTime finalNewExpiryIst, ExternalBPP.ExternalAPI.Types.expiryIST = finalNewExpiryIst}
                      Nothing -> do
                        ticket
              case mbNewRouteCode of
                Just (Just newRouteCode, routeDetail) -> do
                  case (routeDetail.fromStopCode, routeDetail.toStopCode) of
                    (Just fromStopCode, Just toStopCode) -> do
                      mbNewRoute <- tryGettingMaybe $ OTPRest.getRouteByRouteId integratedBPPConfig newRouteCode
                      case mbNewRoute of
                        Just newRoute -> do
                          QRouteDetails.updateRoute (Just newRouteCode) (Just newRouteCode) (Just newRoute.longName) (Just newRoute.shortName) journeyLeg.id.getId
                          fromRoute <- (tryGettingArray $ OTPRest.getRouteStopMappingByStopCodeAndRouteCode fromStopCode newRouteCode integratedBPPConfig) <&> listToMaybe
                          toRoute <- (tryGettingArray $ OTPRest.getRouteStopMappingByStopCodeAndRouteCode toStopCode newRouteCode integratedBPPConfig) <&> listToMaybe
                          pure $
                            newTicket
                              { ExternalBPP.ExternalAPI.Types.fromRouteProviderCode = maybe "NANDI" (.providerCode) fromRoute,
                                ExternalBPP.ExternalAPI.Types.toRouteProviderCode = maybe "NANDI" (.providerCode) toRoute
                              }
                        Nothing -> do
                          case vehicleLiveRouteInfo.routeNumber of
                            Just routeShortName -> do
                              QRouteDetails.updateRoute (Just newRouteCode) (Just newRouteCode) Nothing (Just routeShortName) journeyLeg.id.getId
                              fromRoute <- (tryGettingArray $ OTPRest.getRouteStopMappingByStopCodeAndRouteCode fromStopCode newRouteCode integratedBPPConfig) <&> listToMaybe
                              toRoute <- (tryGettingArray $ OTPRest.getRouteStopMappingByStopCodeAndRouteCode toStopCode newRouteCode integratedBPPConfig) <&> listToMaybe
                              pure $
                                newTicket
                                  { ExternalBPP.ExternalAPI.Types.fromRouteProviderCode = maybe "NANDI" (.providerCode) fromRoute,
                                    ExternalBPP.ExternalAPI.Types.toRouteProviderCode = maybe "NANDI" (.providerCode) toRoute
                                  }
                            Nothing -> return newTicket
                    _ -> pure newTicket
                _ -> do
                  pure newTicket
          )
      void $
        mapConcurrently
          ( \ticketPayload -> do
              qrData <- DirectExternalBPP.generateQR integratedBPPConfig ticketPayload
              QFRFSTicket.udpateQrDataAndValidTill qrData ticketPayload.expiryIST ticketBookingId ticketPayload.ticketNumber
          )
          updatedTickets
      case updatedTickets of
        [] -> pure ()
        (ticket : _) -> do
          let newJourneyExpiry = addUTCTime (-19800) ticket.expiryIST
          QJourney.updateJourneyExpiryTime journey.id $ fromMaybe newJourneyExpiry (max journey.journeyExpiryTime . Just $ newJourneyExpiry)

    tryGettingArray action = do
      resp <- withTryCatch "action:tryGettingArray" action
      case resp of
        Right rightResp -> return rightResp
        Left err -> do
          logError $ "SetOnboarding OTPRest failed: " <> show err
          return []

    tryGettingMaybe action = do
      resp <- withTryCatch "action:tryGettingMaybe" action
      case resp of
        Right rightResp -> return rightResp
        Left err -> do
          logError $ "SetOnboarding OTPRest failed: " <> show err
          return Nothing

postMultimodalUpdateBusLocation ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  ApiTypes.UpdateBusLocationReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalUpdateBusLocation (mbPersonId, _) mbBusOTP req = do
  busOTP <- mbBusOTP & fromMaybeM (InvalidRequest "busOTP query parameter is required")
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  deviceMappings <- QDeviceVehicleMapping.findByVehicleNo busOTP
  deviceMapping <-
    fromMaybeM
      (InvalidRequest "Device not found for provided busOTP")
      (listToMaybe deviceMappings)

  personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId personCityInfo.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound personCityInfo.merchantOperatingCityId.getId)

  now <- getCurrentTime

  let serverTimeEpoch :: Int64
      serverTimeEpoch =
        floor (utcTimeToPOSIXSeconds now)

  let requestTimestampSeconds :: Int64
      requestTimestampSeconds =
        floor (req.timestamp :: Double)

  let kafkaPacket =
        ApiTypes.KafKaPacket
          { lat = req.lat,
            long = req.long,
            timestamp = requestTimestampSeconds,
            deviceId = deviceMapping.deviceId,
            vehicleNumber = busOTP,
            speed = 0.0,
            pushedToKafkaAt = serverTimeEpoch,
            dataState = "L",
            serverTime = serverTimeEpoch,
            provider = "amnex",
            raw = "",
            client_ip = "0.0.0.0",
            ign_status = "ON",
            routeNumber = "101",
            signalQuality = "Good"
          }

  let topicName = riderConfig.kafkaTopicName

  logInfo $ "Pushing bus location to Kafka for deviceId: " <> deviceMapping.deviceId <> " on topic: " <> topicName
  let key = deviceMapping.deviceId

  fork "Pushing bus location to Kafka" $ do
    produceMessage (topicName, Just (TE.encodeUtf8 key)) kafkaPacket
      `catch` \(e :: SomeException) ->
        logError $
          "Failed to push bus location to Kafka: " <> show e

  pure Kernel.Types.APISuccess.Success

postStoreTowerInfo ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  ApiTypes.TowerInfoReq ->
  Flow Kernel.Types.APISuccess.APISuccess
postStoreTowerInfo (mbPersonId, _) req = do
  let personIdStr = maybe "Unknown" (.getId) mbPersonId

  -- Validate coordinates
  validateCoordinates req.userLat req.userLng
  validateAccuracy req.latLngAccuracy

  -- Validate tower info is not empty
  when (null req.towerInfo) $
    throwError $ InvalidRequest "Tower info cannot be empty"

  -- Validate each tower info
  forM_ req.towerInfo $ \tower -> do
    validateCellId tower.cellId
    validateSignalStrength tower.signalStrength
    validateAreaCode tower.areaCode

  logInfo $
    "Received tower info from person: " <> personIdStr
      <> " | Location: ("
      <> show req.userLat
      <> ", "
      <> show req.userLng
      <> ") | Accuracy: "
      <> show req.latLngAccuracy
      <> " | Towers count: "
      <> show (length req.towerInfo)

  let topicName = "tower_info_data_v2"
  let key = personIdStr
  fork "Logging TowerInfo to Kafka" $ do
    produceMessage (topicName, Just (TE.encodeUtf8 key)) req
      `catch` \(e :: SomeException) ->
        logError $
          "Failed to push tower info to Kafka: " <> show e

  return Kernel.Types.APISuccess.Success
  where
    validateCoordinates lat lng = do
      when (lat < -90 || lat > 90) $
        throwError $ InvalidRequest "Invalid latitude: must be between -90 and 90"
      when (lng < -180 || lng > 180) $
        throwError $ InvalidRequest "Invalid longitude: must be between -180 and 180"

    validateAccuracy accuracy = do
      when (accuracy < 0) $
        throwError $ InvalidRequest "Accuracy cannot be negative"

    validateCellId cellId = do
      when (T.null cellId) $
        throwError $ InvalidRequest "Cell ID cannot be empty"

    validateSignalStrength strength = do
      when (strength < -150 || strength > 0) $
        logWarning $ "Unusual signal strength value: " <> show strength

    validateAreaCode areaCode = do
      when (areaCode < 0) $
        logWarning $ "Invalid area code: " <> show areaCode
