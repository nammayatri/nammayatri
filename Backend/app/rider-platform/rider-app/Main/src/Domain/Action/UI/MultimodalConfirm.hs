module Domain.Action.UI.MultimodalConfirm
  ( postMultimodalInitiate,
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
    postMultimodalRouteAvailability,
    postMultimodalSwitchRoute,
    postMultimodalOrderSublegSetOnboardedVehicleDetails,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified API.UI.CancelSearch as CancelSearch
import qualified API.UI.Rating as Rating
import qualified API.UI.Select as Select
import BecknV2.FRFS.Enums
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Monad.Extra (mapMaybeM)
import qualified Data.HashMap.Strict as HashMap
import Data.List (nub, nubBy, partition)
import qualified Data.Text as T
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DEst
import qualified Domain.Types.FRFSTicketBooking as DFRFSB
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
import Environment
import EulerHS.Prelude hiding (all, any, catMaybes, concatMap, elem, find, forM_, groupBy, id, length, map, mapM_, null, sum, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.External.Maps.Google.MapsClient.Types (LatLngV2 (..), LocationV2 (..), WayPointV2 (..))
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude hiding (foldl')
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JL
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyModule.Base
import qualified Lib.JourneyModule.Base as JM
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Lib.JourneyModule.Utils as JLU
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
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
import Tools.Error
import qualified Tools.Error as StationError
import Tools.MultiModal as MM
import qualified Tools.MultiModal as TMultiModal
import qualified Tools.Payment as Payment

postMultimodalInitiate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
postMultimodalInitiate (_personId, _merchantId) journeyId = do
  runAction $ do
    journeyLegs <- QJourneyLeg.getJourneyLegs journeyId
    addAllLegs journeyId (Just journeyLegs) journeyLegs
    journey <- JM.getJourney journeyId
    JM.updateJourneyStatus journey Domain.Types.Journey.INITIATED
    legs <- JM.getAllLegsInfo journey.riderId journeyId
    generateJourneyInfoResponse journey legs
  where
    runAction action = do
      if journeyId.getId == ""
        then action
        else do
          Redis.withWaitAndLockRedis lockKey 10 60 $
            action
    lockKey = "infoLock-" <> journeyId.getId

postMultimodalConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.JourneyConfirmReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalConfirm (_, _) journeyId forcedBookLegOrder journeyConfirmReq = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journey.id
  let confirmElements = journeyConfirmReq.journeyConfirmReqElements
  void $ JM.startJourney journey.riderId confirmElements forcedBookLegOrder journey.id
  -- If all FRFS legs are skipped, update journey status to INPROGRESS. Otherwise, update journey status to CONFIRMED and it would be marked as INPROGRESS on Payment Success in `markJourneyPaymentSuccess`.
  -- Note: INPROGRESS journey status indicates that the tracking has started.
  if isAllFRFSLegSkipped legs confirmElements
    then do
      JM.updateJourneyStatus journey Domain.Types.Journey.INPROGRESS
      fork "Analytics - Journey Skip Without Booking Update" $ QJourney.updateHasStartedTrackingWithoutBooking (Just True) journeyId
    else JM.updateJourneyStatus journey Domain.Types.Journey.CONFIRMED
  fork "Caching recent location" $ JLU.createRecentLocationForMultimodal journey
  pure Kernel.Types.APISuccess.Success
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
  frfsBookingStatusArr <- mapM (FRFSTicketService.frfsBookingStatus (personId, merchantId) True) allJourneyFrfsBookings
  paymentGateWayId <- Payment.fetchGatewayReferenceId merchantId person.merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking
  logInfo $ "paymentGateWayId: " <> show paymentGateWayId
  let anyFirstBooking = listToMaybe frfsBookingStatusArr
      paymentOrder =
        anyFirstBooking >>= (.payment)
          <&> ( \p ->
                  ApiTypes.PaymentOrder {sdkPayload = p.paymentOrder, status = p.status}
              )
      allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyFrfsBookings
  let paymentFareUpdate =
        catMaybes $
          map
            ( \booking -> do
                let leg = find (\l -> l.legSearchId == Just booking.searchId.getId) legs
                if fromMaybe False booking.isFareChanged
                  then case leg <&> (.sequenceNumber) of
                    Just journeyLegOrder ->
                      Just $
                        ApiTypes.PaymentFareUpdate
                          { journeyLegOrder,
                            oldFare = mkPriceAPIEntity booking.estimatedPrice,
                            newFare = mkPriceAPIEntity booking.price
                          }
                    Nothing -> Nothing
                  else Nothing
            )
            allJourneyFrfsBookings
  if allLegsOnInitDone
    then do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder,
            paymentFareUpdate = Just paymentFareUpdate,
            gatewayReferenceId = paymentGateWayId
          }
    else do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder = Nothing,
            paymentFareUpdate = Nothing,
            gatewayReferenceId = paymentGateWayId
          }

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
        Select.Success -> return ()
        Select.BookingAlreadyCreated -> throwError (InternalError $ "Cannot cancel search as booking is already created for searchId: " <> show legSearchId)
        Select.FailedToCancel -> throwError (InvalidRequest $ "Failed to cancel search for searchId: " <> show legSearchId)

postMultimodalOrderSwitchFRFSTier ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.SwitchFRFSTierReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSwitchFRFSTier (mbPersonId, merchantId) journeyId legOrder req = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")

  whenJust journeyLeg.legSearchId $ \legSearchId -> do
    mbAlternateShortNames <- getAlternateShortNames
    let searchId = Id legSearchId
    QJourneyLeg.updateLegPricingIdByLegSearchId (Just req.quoteId.getId) journeyLeg.legSearchId
    mbBooking <- QFRFSTicketBooking.findBySearchId searchId
    whenJust mbBooking $ \booking -> do
      quote <- QFRFSQuote.findById req.quoteId >>= fromMaybeM (InvalidRequest "Quote not found")
      let quantity = booking.quantity
          childTicketQuantity = fromMaybe 0 booking.childTicketQuantity
          quantityRational = fromIntegral quantity :: Rational
          childTicketQuantityRational = fromIntegral childTicketQuantity :: Rational
          childPrice = fromMaybe quote.price quote.childPrice
          totalPriceForSwitchLeg =
            Price
              { amount = HighPrecMoney $ (quote.price.amount.getHighPrecMoney * quantityRational) + (childPrice.amount.getHighPrecMoney * childTicketQuantityRational),
                amountInt = Money $ (quote.price.amountInt.getMoney * quantity) + (childPrice.amountInt.getMoney * childTicketQuantity),
                currency = quote.price.currency
              }
          updatedBooking =
            booking
              { DFRFSB.quoteId = req.quoteId,
                DFRFSB.price = totalPriceForSwitchLeg,
                DFRFSB.estimatedPrice = totalPriceForSwitchLeg
              }
      void $ QFRFSTicketBooking.updateByPrimaryKey updatedBooking
    whenJust mbAlternateShortNames $ \alternateShortNames -> do
      QRouteDetails.updateAlternateShortNames alternateShortNames journeyLeg.id.getId
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
  where
    getAlternateShortNames :: Flow (Maybe [Text])
    getAlternateShortNames = do
      options <- getMultimodalOrderGetLegTierOptions (mbPersonId, merchantId) journeyId legOrder
      let mbSelectedOption = find (\option -> option.quoteId == Just req.quoteId) options.options
      return $ mbSelectedOption <&> (.availableRoutes)

postMultimodalSwitch ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.SwitchLegReq ->
  Environment.Flow ApiTypes.JourneyInfoResp
postMultimodalSwitch userInfo@(mbPersonId, _) journeyId req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Invalid person id")
  JM.switchLeg journeyId personId req
  getMultimodalBookingInfo userInfo journeyId

postMultimodalRiderLocation ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.RiderLocationReq ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalRiderLocation personOrMerchantId journeyId req = do
  addPoint journeyId req
  journeyStatus <- getMultimodalJourneyStatus personOrMerchantId journeyId
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
  JM.extendLeg journeyId req.startLocation req.endLocation Nothing req.fare req.distance req.duration req.bookingUpdateRequestId
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
  void $ JM.startJourneyLeg leg
  pure Kernel.Types.APISuccess.Success

getMultimodalJourneyStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatusResp
getMultimodalJourneyStatus (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsStatus journey
  generateJourneyStatusResponse journey legs

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
                let feedbackReq = Rating.FeedbackReq {rideId = ride.id, rating = ratingValue, wasRideSafe = Nothing, shouldFavDriver = Nothing, wasOfferedAssistance = Nothing, feedbackDetails = journeyFeedbackForm.additionalFeedBack, mbAudio = Nothing}
                try @_ @SomeException (Rating.processRating (riderId, merchantId) feedbackReq)
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
          try @_ @SomeException (FRFSTicketService.postFrfsBookingFeedback (mbPersonId, merchantId) booking.id (FRFSTicketService.BookingFeedback FRFSTicketService.BookingFeedbackReq {feedbackDetails = feedbackDetails}))
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

getPublicTransportData ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe VehicleCategory ->
    Environment.Flow API.Types.UI.MultimodalConfirm.PublicTransportData
  )
getPublicTransportData (mbPersonId, merchantId) mbCity _mbConfigVersion mbVehicleNumber mbVehicleType = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbRequestCity <- maybe (pure Nothing) (CQMOC.findByMerchantIdAndCity merchantId) mbCity
  let merchantOperatingCityId = maybe person.merchantOperatingCityId (.id) mbRequestCity
  let vehicleTypes =
        case mbVehicleType of
          Just BUS -> [Enums.BUS]
          Just METRO -> [Enums.METRO]
          Just SUBWAY -> [Enums.SUBWAY]
          _ -> [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes
  mbVehicleLiveInfo <-
    case mbVehicleNumber of
      Just vehicleNumber -> do
        vehicleLiveInfo <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber >>= fromMaybeM (InvalidVehicleNumber $ "Vehicle " <> vehicleNumber <> ", not found on any route")
        return $ Just vehicleLiveInfo
      Nothing -> return Nothing

  let mkResponse stations routes routeStops bppConfig = do
        gtfsVersion <-
          try @_ @SomeException (OTPRest.getGtfsVersion bppConfig) >>= \case
            Left _ -> return bppConfig.feedKey
            Right gtfsVersion -> return gtfsVersion
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
              ptcv = gtfsVersion
            }

  let fetchData bppConfig = do
        case mbVehicleLiveInfo of
          Just vehicleLiveInfo -> do
            try @_ @SomeException
              ( do
                  routes <- maybeToList <$> OTPRest.getRouteByRouteId bppConfig vehicleLiveInfo.routeCode
                  routeStopMappingInMem <- OTPRest.getRouteStopMappingByRouteCodeInMem vehicleLiveInfo.routeCode bppConfig
                  routeStops <- OTPRest.parseRouteStopMappingInMemoryServer routeStopMappingInMem bppConfig bppConfig.merchantId bppConfig.merchantOperatingCityId
                  stations <- OTPRest.parseStationsFromInMemoryServer routeStopMappingInMem bppConfig
                  mkResponse stations routes routeStops bppConfig
              )
              >>= \case
                Left err -> throwError (PublicTransportDataUnavailable $ "Public transport data unavailable: " <> show err)
                Right response -> return response
          Nothing -> do
            stations <- OTPRest.getStationsByGtfsId bppConfig
            routes <- OTPRest.getRoutesByGtfsId bppConfig
            mkResponse stations routes ([] :: [DRSM.RouteStopMapping]) bppConfig

  transportDataList <-
    try @_ @SomeException
      ( mapM
          ( \config -> do
              baseUrl <- MM.getOTPRestServiceReq config.merchantId config.merchantOperatingCityId
              return (config, (config.feedKey, baseUrl))
          )
          integratedBPPConfigs
      )
      >>= \case
        Left _ -> mapM fetchData integratedBPPConfigs
        Right configsWithFeedInfo -> do
          -- Group configs by feed_id and take first config for each feed_id
          let configsByFeedId = HashMap.fromListWith (++) $ map (\(config, (feedKey, _)) -> (feedKey, [config])) configsWithFeedInfo
              uniqueConfigs = map (head . snd) $ HashMap.toList configsByFeedId
          mapM fetchData uniqueConfigs

  gtfsVersion <-
    try @_ @SomeException (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
      Left _ -> return (map (.feedKey) integratedBPPConfigs)
      Right gtfsVersions -> return gtfsVersions
  let transportData =
        ApiTypes.PublicTransportData
          { ss = concatMap (.ss) transportDataList,
            rs = concatMap (.rs) transportDataList,
            rsm = concatMap (.rsm) transportDataList,
            ptcv = T.intercalate (T.pack "#") gtfsVersion
          }
  return transportData

getMultimodalOrderGetLegTierOptions ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Int ->
  Environment.Flow ApiTypes.LegServiceTierOptionsResp
getMultimodalOrderGetLegTierOptions (mbPersonId, merchantId) journeyId legOrder = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  legs <- QJourneyLeg.getJourneyLegs journeyId
  now <- getCurrentTime
  journeyLegInfo <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let mbAgencyId = journeyLegInfo.agency >>= (.gtfsId)
  let vehicleCategory = castTravelModeToVehicleCategory journeyLegInfo.mode
  quotes <- maybe (pure []) (QFRFSQuote.findAllBySearchId . Id) journeyLegInfo.legSearchId
  let availableServiceTiers = mapMaybe JMTypes.getServiceTierFromQuote quotes
  mbIntegratedBPPConfig <- SIBC.findMaybeIntegratedBPPConfigFromAgency mbAgencyId person.merchantOperatingCityId vehicleCategory DIBC.MULTIMODAL
  let mbRouteDetail = journeyLegInfo.routeDetails & listToMaybe
  let mbFomStopCode = mbRouteDetail >>= (.fromStopCode)
  let mbToStopCode = mbRouteDetail >>= (.toStopCode)
  let mbArrivalTime = mbRouteDetail >>= (.fromArrivalTime)
  let arrivalTime = fromMaybe now mbArrivalTime

  case (mbFomStopCode, mbToStopCode, mbIntegratedBPPConfig) of
    (Just fromStopCode, Just toStopCode, Just integratedBPPConfig) -> do
      (_, availableRoutesByTiers, _) <- JLU.findPossibleRoutes (Just availableServiceTiers) fromStopCode toStopCode arrivalTime integratedBPPConfig merchantId person.merchantOperatingCityId vehicleCategory (vehicleCategory /= Enums.SUBWAY) False
      return $ ApiTypes.LegServiceTierOptionsResp {options = availableRoutesByTiers}
    _ -> return $ ApiTypes.LegServiceTierOptionsResp {options = []}

castTravelModeToVehicleCategory :: DTrip.MultimodalTravelMode -> Enums.VehicleCategory
castTravelModeToVehicleCategory DTrip.Bus = Enums.BUS
castTravelModeToVehicleCategory DTrip.Taxi = Enums.AUTO_RICKSHAW
castTravelModeToVehicleCategory DTrip.Walk = Enums.AUTO_RICKSHAW
castTravelModeToVehicleCategory DTrip.Metro = Enums.METRO
castTravelModeToVehicleCategory DTrip.Subway = Enums.SUBWAY

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

-- Helper function to mark all sub-legs as completed for FRFS legs
markAllSubLegsCompleted :: DJourneyLeg.JourneyLeg -> UTCTime -> Environment.Flow ()
markAllSubLegsCompleted journeyLeg trackingStatusUpdateTime = do
  let subLegOrders = map (\r -> fromMaybe 1 r.subLegOrder) journeyLeg.routeDetails
  case subLegOrders of
    [] -> markLegStatus (Just JL.Completed) (Just JMState.Finished) journeyLeg Nothing trackingStatusUpdateTime
    orders -> mapM_ (\subOrder -> markLegStatus (Just JL.Completed) (Just JMState.Finished) journeyLeg (Just subOrder) trackingStatusUpdateTime) orders

postMultimodalComplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalComplete (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  cancelOngoingTaxiLegs legs
  now <- getCurrentTime
  mapM_ (\leg -> markAllSubLegsCompleted leg now) legs
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey updatedLegStatus
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
            (_, tiers, _) <- JLU.findPossibleRoutes Nothing fromStopCode toStopCode arrivalTime integratedBPPConfig merchantId person.merchantOperatingCityId vehicleCategory True False
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
              journeyLegId = leg.id
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
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing reqJourneyLeg.merchantOperatingCityId (Utils.frfsVehicleCategoryToBecknVehicleCategory Spec.METRO) DIBC.MULTIMODAL
  riderConfig <-
    QRC.findByMerchantOperatingCityId reqJourneyLeg.merchantOperatingCityId Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist reqJourneyLeg.merchantOperatingCityId.getId)

  (newLeg, mbSourceStation, mbDestStation) <- processChangeStops journey reqJourneyLeg integratedBPPConfig riderConfig

  let (prevLeg, nextLeg) = JMU.findAdjacentLegs reqJourneyLeg.sequenceNumber allLegs
  mbGates <- updateLegsWithGates riderConfig prevLeg nextLeg reqJourneyLeg newLeg journey.merchantId reqJourneyLeg.merchantOperatingCityId mbSourceStation mbDestStation

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
      reqJourneyLeg.finalBoardedBusNumber

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
        DTrip.Metro -> validateAndGetMetroLeg sourceLatLong destLatLong sourceStopCode destStopCode journey reqJourneyLeg riderConfig
        _ -> throwError $ InvalidRequest "Mode not implemented for station changes"

    validateAndGetMetroLeg ::
      (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
      LatLong -> -- Source location
      LatLong -> -- Destination location
      Text -> -- Source stop code
      Text -> -- Destination stop code
      Domain.Types.Journey.Journey ->
      DJourneyLeg.JourneyLeg ->
      DRC.RiderConfig ->
      m MultiModalTypes.MultiModalLeg
    validateAndGetMetroLeg sourceLatLong destLatLong sourceStopCode destStopCode journey reqJourneyLeg riderConfig = do
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
                sortingType = MultiModal.Fastest
              }

      transitServiceReq <- TMultiModal.getTransitServiceReq journey.merchantId reqJourneyLeg.merchantOperatingCityId
      otpResponse <- JMU.measureLatency (MultiModal.getTransitRoutes (Just journeyId.getId) transitServiceReq transitRoutesReq >>= fromMaybeM (OTPServiceUnavailable "No routes found from OTP")) "getTransitRoutes"

      validatedRoute <- JMU.getBestOneWayRoute MultiModalTypes.MetroRail otpResponse.routes (Just sourceStopCode) (Just destStopCode) & fromMaybeM (NoValidMetroRoute sourceStopCode destStopCode)

      metroLeg <- case filter (\leg -> leg.mode == MultiModalTypes.MetroRail) validatedRoute.legs of
        [singleMetroLeg] -> return singleMetroLeg
        _ -> throwError $ MetroLegNotFound "Multiple metro legs found in route"
      return metroLeg

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

  availableServiceTiers <-
    case (req.journeyId, req.legOrder) of
      (Just journeyId, Just legOrder) -> do
        journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
        quotes <- maybe (pure []) (QFRFSQuote.findAllBySearchId . Id) journeyLeg.legSearchId
        return $ Just $ mapMaybe JMTypes.getServiceTierFromQuote quotes
      _ -> pure Nothing

  case integratedBPPConfigs of
    [] -> return $ ApiTypes.RouteAvailabilityResp {availableRoutes = []}
    (integratedBPPConfig : _) -> do
      -- Use findPossibleRoutes to get available routes
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

      let (liveBuses, staticBuses) = partition (\route -> route.source == LIVE) availableRoutesByTier
      -- Filter routes based on onlyLive flag if needed
      let filteredRoutes =
            if req.onlyLive
              then liveBuses
              else liveBuses <> staticBuses

      -- Convert to API response format
      availableRoutes <- concatMapM (convertToAvailableRoute integratedBPPConfig person) filteredRoutes

      return $ ApiTypes.RouteAvailabilityResp {availableRoutes = availableRoutes}
  where
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
  journey <- JM.getJourney journeyId
  journeyLeg <- QJourneyLeg.getJourneyLeg journeyId legOrder
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId journey.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound journey.merchantOperatingCityId.getId)
  if riderConfig.validateSetOnboardingVehicleRequest == Just True
    then do
      let journeyLegRouteCodes = mapMaybe (.routeCode) journeyLeg.routeDetails
      vehicleType <-
        case journeyLeg.mode of
          DTrip.Bus -> return Enums.BUS
          DTrip.Metro -> return Enums.METRO
          DTrip.Subway -> return Enums.SUBWAY
          _ -> throwError $ UnsupportedVehicleType (show journeyLeg.mode)
      integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig journey.merchantOperatingCityId vehicleType DIBC.MULTIMODAL
      vehicleLiveRouteInfo <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber >>= fromMaybeM (VehicleUnserviceableOnRoute "Vehicle not found on any route")
      unless (vehicleLiveRouteInfo.routeCode `elem` journeyLegRouteCodes) $
        throwError $ VehicleUnserviceableOnRoute ("Vehicle " <> vehicleNumber <> ", the route code " <> vehicleLiveRouteInfo.routeCode <> ", not found on any route: " <> show journeyLegRouteCodes)
      unless (maybe False (\legServiceTypes -> vehicleLiveRouteInfo.serviceType `elem` legServiceTypes) journeyLeg.serviceTypes) $
        throwError $ VehicleServiceTierUnserviceable ("Vehicle " <> vehicleNumber <> ", the service tier" <> show vehicleLiveRouteInfo.serviceType <> ", not found on any route: " <> show journeyLeg.serviceTypes)
    else pure ()
  QJourneyLeg.updateByPrimaryKey $ journeyLeg {DJourneyLeg.finalBoardedBusNumber = Just vehicleNumber}
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
