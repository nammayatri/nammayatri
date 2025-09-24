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
    postMultimodalSetRouteName,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified API.UI.CancelSearch as CancelSearch
import qualified API.UI.Rating as Rating
import BecknV2.FRFS.Enums
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Monad.Extra (mapMaybeM)
import qualified Data.HashMap.Strict as HashMap
import Data.List (nub, nubBy, partition)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
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
import Domain.Utils (mapConcurrently)
import Environment
import EulerHS.Prelude hiding (all, any, catMaybes, concatMap, elem, find, forM_, groupBy, id, length, map, mapM_, null, sum, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import qualified ExternalBPP.ExternalAPI.CallAPI as DirectExternalBPP
import qualified ExternalBPP.ExternalAPI.Types
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
import qualified SharedLogic.Cancel as SharedCancel
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
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
import qualified Tools.Metrics as Metrics
import Tools.MultiModal as MM
import qualified Tools.MultiModal as TMultiModal
import qualified Tools.Payment as Payment
import Utils.Utils (decodeFromText)

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
                  ApiTypes.PaymentOrder
                    { sdkPayload = p.paymentOrder,
                      status =
                        if any (\b -> (b.payment <&> (.status)) == Just FRFSTicketService.SUCCESS) frfsBookingStatusArr
                          then FRFSTicketService.SUCCESS
                          else p.status
                    }
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
postMultimodalOrderSwitchFRFSTier (mbPersonId, merchantId) journeyId legOrder req = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")

  whenJust journeyLeg.legSearchId $ \legSearchId -> do
    mbAlternateShortNames <- getAlternateRouteInfo
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
    whenJust mbAlternateShortNames $ \(alternateShortNames, alternateRouteIds) -> do
      QRouteDetails.updateAlternateShortNamesAndRouteIds alternateShortNames (Just alternateRouteIds) journeyLeg.id.getId
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
  where
    getAlternateRouteInfo :: Flow (Maybe ([Text], [Text]))
    getAlternateRouteInfo = do
      options <- getMultimodalOrderGetLegTierOptions (mbPersonId, merchantId) journeyId legOrder
      let mbSelectedOption = find (\option -> option.quoteId == Just req.quoteId) options.options
      return $ mbSelectedOption <&> (unzip . map (\a -> (a.shortName, a.routeCode)) . (.availableRoutesInfo))

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
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId
  let vehicleTypes =
        case mbVehicleType of
          Just BUS -> [Enums.BUS]
          Just METRO -> [Enums.METRO]
          Just SUBWAY -> [Enums.SUBWAY]
          _ -> [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  whenJust mbVehicleNumber $ \vehicleNumber -> do
    Metrics.incrementBusScannetCounterMetric "ANNA_APP" merchantOperatingCityId.getId vehicleNumber

  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes
  mbVehicleLiveRouteInfo <-
    case mbVehicleNumber of
      Just vehicleNumber -> do
        vehicleRouteInfo <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber >>= fromMaybeM (InvalidVehicleNumber $ "Vehicle " <> vehicleNumber <> ", not found on any route")
        pure $ Just vehicleRouteInfo
      Nothing -> return Nothing

  let mbOppositeTripDetails :: Maybe [NandiTypes.BusScheduleTrip] =
        mbVehicleLiveRouteInfo
          <&> \(_, vehicleLiveRouteInfo) -> do
            ( if vehicleLiveRouteInfo.tripNumber == 1
                then take 2
                else take 1
              )
              $ sortOn (.stops_count) $ filter (\remainingTrip -> remainingTrip.route_number == vehicleLiveRouteInfo.routeNumber && remainingTrip.route_id /= vehicleLiveRouteInfo.routeCode) (fromMaybe [] vehicleLiveRouteInfo.remaining_trip_details)

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

  let fetchData mbRouteCode bppConfig = do
        case mbRouteCode of
          Just routeCode -> do
            try @_ @SomeException
              ( do
                  routes <- maybeToList <$> OTPRest.getRouteByRouteId bppConfig routeCode
                  routeStopMappingInMem <- OTPRest.getRouteStopMappingByRouteCodeInMem routeCode bppConfig
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
        Left _ -> do
          lst1 <- mapConcurrently (fetchData (((.routeCode) . snd) <$> mbVehicleLiveRouteInfo)) integratedBPPConfigs
          lst2 <-
            maybe
              (pure [])
              ( \oppositeTripDetails ->
                  concat
                    <$> mapConcurrently
                      (\oppositeTripDetail -> mapConcurrently (fetchData (Just oppositeTripDetail.route_id)) integratedBPPConfigs)
                      oppositeTripDetails
              )
              mbOppositeTripDetails
          return (lst1 <> lst2)
        Right configsWithFeedInfo -> do
          -- Group configs by feed_id and take first config for each feed_id
          let configsByFeedId = HashMap.fromListWith (++) $ map (\(config, (feedKey, _)) -> (feedKey, [config])) configsWithFeedInfo
              uniqueConfigs = map (head . snd) $ HashMap.toList configsByFeedId
          lst1 <- mapConcurrently (fetchData (((.routeCode) . snd) <$> mbVehicleLiveRouteInfo)) uniqueConfigs
          lst2 <- maybe (pure []) (\oppositeTripDetails -> concat <$> mapConcurrently (\oppositeTripDetail -> mapConcurrently (fetchData (Just oppositeTripDetail.route_id)) uniqueConfigs) oppositeTripDetails) mbOppositeTripDetails
          return (lst1 <> lst2)

  gtfsVersion <-
    try @_ @SomeException (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
      Left _ -> return (map (.feedKey) integratedBPPConfigs)
      Right gtfsVersions -> return gtfsVersions
  let transportData =
        ApiTypes.PublicTransportData
          { ss = concatMap (.ss) transportDataList,
            rs = concatMap (.rs) transportDataList,
            rsm = concatMap (.rsm) transportDataList,
            ptcv = T.intercalate (T.pack "#") gtfsVersion <> (maybe "" (\version -> "#" <> show version) (riderConfig >>= (.domainPublicTransportDataVersion)))
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

      options <- getMultimodalOrderGetLegTierOptions userInfo' journeyId legOrder
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
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing reqJourneyLeg.merchantOperatingCityId (Utils.frfsVehicleCategoryToBecknVehicleCategory Spec.METRO) DIBC.MULTIMODAL
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
            updateSource = reqJourneyLeg.finalBoardedBusNumberSource
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
  (integratedBPPConfig, vehicleLiveRouteInfo) <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber >>= fromMaybeM (VehicleUnserviceableOnRoute "Vehicle not found on any route")
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId journey.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound journey.merchantOperatingCityId.getId)
  let serviceTierRelationshipCfg = riderConfig.serviceTierRelationshipCfg
  -- looks like this as of now 😢, should have one element -> [{"code":"3880","color":null,"endPoint":{"lat":13.05177,"lon":80.09496},"longName":"REDHILLS BUS TERMINUS To POONAMALLEE B.T","priceWithCurrency":{"amount":25,"currency":"INR"},"sequenceNum":null,"shortName":"62","startPoint":{"lat":13.19305,"lon":80.18437},"stations":[{"address":null,"code":"tJbXQuOE","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.13037,"lon":80.15852,"name":"PUDUR","routeCodes":null,"sequenceNum":17,"stationType":"START","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"CpULuhdq","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12724,"lon":80.1539,"name":"AMBATTUR ORAGADAM","routeCodes":null,"sequenceNum":18,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"wFSILCzc","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12182,"lon":80.14794,"name":"RAAKI THEATRE","routeCodes":null,"sequenceNum":19,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"FlYjxOxQ","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12446,"lon":80.14361,"name":"THIRUMULLAIVOYAL STEDFORD HOSPITAL","routeCodes":null,"sequenceNum":20,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"GkYZbgqg","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.1281,"lon":80.13972,"name":"THIRUMULLAIVOYAL SARASWATHI NAGAR CTH SALAI","routeCodes":null,"sequenceNum":21,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"FStgRiBS","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.13049,"lon":80.13634,"name":"THIRUMULLAIVOYAL MANIKANDAPURAM","routeCodes":null,"sequenceNum":22,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"XiLvnZTk","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.13073,"lon":80.13103,"name":"THIRUMULLAIVOYAL","routeCodes":null,"sequenceNum":23,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"Vlipnutt","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12964,"lon":80.12503,"name":"THIRUMULLAIVOYAL VAISHNAVI NAGAR","routeCodes":null,"sequenceNum":24,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"bHagonKv","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.1258,"lon":80.11913,"name":"MURUGAPPA POLYTECHNIC","routeCodes":null,"sequenceNum":25,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"PhjcDdmq","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.12338,"lon":80.1125,"name":"AVADI TUBE PRODUCTS OF INDIA","routeCodes":null,"sequenceNum":26,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"vacPrbQA","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11972,"lon":80.10169,"name":"AVADI BUS TERMINUS","routeCodes":null,"sequenceNum":27,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"SGnJOquj","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11942,"lon":80.096,"name":"AVADI CHECK POST","routeCodes":null,"sequenceNum":28,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"pGHnvifU","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11704,"lon":80.09786,"name":"AVADI GOVERNMENT HOSPITAL","routeCodes":null,"sequenceNum":29,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"qNiTyrkl","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11658,"lon":80.10061,"name":"KANNIGAPURAM AVADI","routeCodes":null,"sequenceNum":30,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"FldHcjZC","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11575,"lon":80.10516,"name":"AVADI MARKET","routeCodes":null,"sequenceNum":31,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"9d18d389df1961e2f3d3c19e315bf99a","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.11158,"lon":80.10868,"name":"AVADI JB ESTATE","routeCodes":null,"sequenceNum":32,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"f8687de5e5e79549349ff84b70dda40c","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.10552,"lon":80.10853,"name":"AVADI VASANTHAM NAGAR","routeCodes":null,"sequenceNum":33,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"ae4f951daf0a29a3e562512b767b0a24","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.10265,"lon":80.10843,"name":"AVADI MOORTHY NAGAR","routeCodes":null,"sequenceNum":34,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"BJDTCJYP","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.09841,"lon":80.10839,"name":"GOVARDANAGIRI","routeCodes":null,"sequenceNum":35,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"VMzKgUSR","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.09361,"lon":80.1085,"name":"IYANKULAM","routeCodes":null,"sequenceNum":36,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"GJEXlFdt","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.08655,"lon":80.10869,"name":"PARUTHIPATTU ROAD JUNCTION","routeCodes":null,"sequenceNum":37,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"dgLtpUgu","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.08167,"lon":80.10932,"name":"MELPAKKAM","routeCodes":null,"sequenceNum":38,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"NRaiNGwl","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.07822,"lon":80.11062,"name":"KADUVETTI","routeCodes":null,"sequenceNum":39,"stationType":"INTERMEDIATE","timeTakenToTravelUpcomingStop":null,"towards":null},{"address":null,"code":"qLQHuyIe","color":null,"distance":null,"integratedBppConfigId":"4f148691-12cc-42a7-b49e-f9bd86863fa1","lat":13.06628,"lon":80.11255,"name":"SA ENGINEERING COLLEGE","routeCodes":null,"sequenceNum":40,"stationType":"END","timeTakenToTravelUpcomingStop":null,"towards":null}],"travelTime":null,"vehicleServiceTier":{"_type":"EXECUTIVE","description":"DELUX BUSES","isAirConditioned":false,"longName":"DELUX BUSES","providerCode":"S","shortName":"DELUX BUSES"}}]
  let mbServiceTier :: Maybe FRFSTicketService.FRFSVehicleServiceTierAPI = listToMaybe =<< (.vehicleServiceTier) =<< decodeFromText =<< quote.routeStationsJson
  case mbServiceTier of
    Just serviceTier -> do
      unless (vehicleLiveRouteInfo.serviceType == serviceTier._type) $
        throwError $ VehicleServiceTierUnserviceable ("Vehicle " <> vehicleNumber <> ", the service tier" <> show vehicleLiveRouteInfo.serviceType <> ", not found on any route: " <> show journeyLeg.serviceTypes)
    Nothing -> do
      -- todo: MERTRICS add metric here
      logError $ "CRITICAL: Service tier not found for vehicle, skipping validation " <> vehicleNumber
  let journeyLegRouteCodes = nub (mapMaybe (.routeCode) journeyLeg.routeDetails <> (concat $ mapMaybe (.alternateRouteIds) journeyLeg.routeDetails))
  unless (vehicleLiveRouteInfo.routeCode `elem` journeyLegRouteCodes) $ do
    logError $ "Vehicle " <> vehicleNumber <> ", the route code " <> vehicleLiveRouteInfo.routeCode <> ", not found on any route: " <> show journeyLegRouteCodes <> ", Please board the bus moving on allowed possible Routes for the booking."
    when (riderConfig.validateSetOnboardingVehicleRequest == Just True) $
      throwError $ VehicleUnserviceableOnRoute ("Vehicle " <> vehicleNumber <> ", the route code " <> vehicleLiveRouteInfo.routeCode <> ", not found on any route: " <> show journeyLegRouteCodes <> ", Please board the bus moving on allowed possible Routes for the booking.")
  let mbNewRouteCode = (vehicleLiveRouteInfo.routeCode,) <$> (listToMaybe journeyLeg.routeDetails) -- doing list to maybe as onluy need from and to stop codes, which will be same in all tickets
  updateTicketQRData journey journeyLeg riderConfig integratedBPPConfig booking.id mbNewRouteCode
  QJourneyLeg.updateByPrimaryKey $
    journeyLeg
      { DJourneyLeg.finalBoardedBusNumber = Just vehicleNumber,
        DJourneyLeg.finalBoardedBusNumberSource = Just DJourneyLeg.UserActivated,
        DJourneyLeg.finalBoardedDepotNo = vehicleLiveRouteInfo.depot,
        DJourneyLeg.finalBoardedWaybillId = vehicleLiveRouteInfo.waybillId,
        DJourneyLeg.finalBoardedScheduleNo = Just vehicleLiveRouteInfo.scheduleNo
      }
  updatedLegs <- JM.getAllLegsInfo journey.riderId journeyId
  generateJourneyInfoResponse journey updatedLegs
  where
    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" utcTime

    parseUtcTime :: Text -> Maybe UTCTime
    parseUtcTime t = parseTimeM True defaultTimeLocale "%d-%m-%Y %H:%M:%S" (T.unpack t)

    updateTicketQRData journey journeyLeg riderConfig integratedBPPConfig ticketBookingId mbNewRouteCode = do
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
                Just (newRouteCode, routeDetail) -> do
                  case (routeDetail.fromStopCode, routeDetail.toStopCode) of
                    (Just fromStopCode, Just toStopCode) -> do
                      mbNewRoute <- OTPRest.getRouteByRouteId integratedBPPConfig newRouteCode
                      QRouteDetails.updateRoute (Just newRouteCode) (Just newRouteCode) ((.longName) <$> mbNewRoute) ((.shortName) <$> mbNewRoute) journeyLeg.id.getId
                      fromRoute <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode fromStopCode newRouteCode integratedBPPConfig <&> listToMaybe
                      toRoute <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode toStopCode newRouteCode integratedBPPConfig <&> listToMaybe
                      pure $
                        newTicket
                          { ExternalBPP.ExternalAPI.Types.fromRouteProviderCode = maybe "NANDI" (.providerCode) fromRoute,
                            ExternalBPP.ExternalAPI.Types.toRouteProviderCode = maybe "NANDI" (.providerCode) toRoute
                          }
                    _ -> pure newTicket
                Nothing -> do
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
