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
    getActiveJourneyIds,
    getMultimodalFeedback,
    getMultimodalUserPreferences,
    postMultimodalUserPreferences,
    postMultimodalTransitOptionsLite,
    postMultimodalOrderSwitchFRFSTier,
    getPublicTransportData,
    getMultimodalOrderGetLegTierOptions,
    postMultimodalPaymentUpdateOrder,
    postMultimodalOrderSublegSetStatus,
    postMultimodalOrderSublegSetStatusV2,
    postMultimodalTicketVerify,
    postMultimodalComplete,
    postMultimodalOrderSoftCancel,
    getMultimodalOrderCancelStatus,
    postMultimodalOrderCancel,
    getMultimodalOrderSimilarJourneyLegs,
    postMultimodalOrderSwitchJourneyLeg,
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
import Data.List (nub, nubBy)
import qualified Data.Text as T
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DEst
import qualified Domain.Types.FRFSTicketBooking as DFRFSB
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSB
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyFeedback as JFB
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person
import qualified Domain.Types.RouteDetails as RD
import qualified Domain.Types.SearchRequest as DSR
import Environment
import EulerHS.Prelude hiding (all, any, catMaybes, concatMap, elem, find, forM_, groupBy, id, length, map, mapM_, null, sum, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude hiding (foldl')
import qualified Kernel.Storage.Hedis as Redis
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
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import Storage.Queries.FRFSSearch as QFRFSSearch
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Storage.Queries.Journey as QJourney
import Storage.Queries.JourneyFeedback as SQJFB
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.JourneyLegMapping as QJourneyLegMapping
import Storage.Queries.MultimodalPreferences as QMP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderConfig as QRiderConfig
import qualified Storage.Queries.RouteDetails as QRouteDetails
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.MultiModal as MM
import qualified Tools.Payment as Payment

validateMetroBusinessHours :: Id Domain.Types.Journey.Journey -> Environment.Flow ()
validateMetroBusinessHours journeyId = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  riderConfig <- QRC.findByMerchantOperatingCityId journey.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist journey.merchantOperatingCityId.getId)
  now <- getCurrentTime
  let isOutsideMetroBusinessHours = case (riderConfig.qrTicketRestrictionStartTime, riderConfig.qrTicketRestrictionEndTime) of
        (Just start, Just end) -> JM.isWithinTimeBound start end now riderConfig.timeDiffFromUtc
        _ -> False
      hasMetroLeg = any (\leg -> leg.mode == DTrip.Metro) legs
  when (hasMetroLeg && isOutsideMetroBusinessHours) $
    throwError $ InvalidRequest "Metro booking not allowed outside business hours"

postMultimodalInitiate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
postMultimodalInitiate (_personId, _merchantId) journeyId = do
  Redis.withLockRedisAndReturnValue lockKey 60 $ do
    validateMetroBusinessHours journeyId
    journeyLegs <- QJourneyLeg.getJourneyLegs journeyId
    addAllLegs journeyId (Just journeyLegs) journeyLegs
    journey <- JM.getJourney journeyId
    JM.updateJourneyStatus journey Domain.Types.Journey.INITIATED
    legs <- JM.getAllLegsInfo journey.riderId journeyId
    generateJourneyInfoResponse journey legs
  where
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
postMultimodalConfirm (mbPersonId, _) journeyId forcedBookLegOrder journeyConfirmReq = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  validateMetroBusinessHours journeyId
  journey <- JM.getJourney journeyId
  let confirmElements = journeyConfirmReq.journeyConfirmReqElements
  forM_ confirmElements $ \element -> do
    when element.skipBooking $ JM.skipLeg journeyId element.journeyLegOrder True personId
  void $ JM.startJourney journey.riderId confirmElements forcedBookLegOrder journey.id
  JM.updateJourneyStatus journey Domain.Types.Journey.CONFIRMED
  fork "Caching recent location" $ JLU.createRecentLocationForMultimodal journey
  pure Kernel.Types.APISuccess.Success

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
    mbEstimate <- maybe (pure Nothing) (QEstimate.findById . Id) (searchReq.journeyLegInfo >>= (.pricingId))
    whenJust mbEstimate $ \estimate -> do
      when (estimate.status `elem` [DEst.COMPLETED, DEst.CANCELLED, DEst.GOT_DRIVER_QUOTE, DEst.DRIVER_QUOTE_CANCELLED]) $
        throwError $ InvalidRequest "Can't switch vehicle if driver has already being assigned"
      QSearchRequest.updatePricingId (Id legSearchId) (Just req.estimateId.getId)
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
    QFRFSSearch.updatePricingId searchId (Just req.quoteId.getId)
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

getActiveJourneyIds ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m [Domain.Types.Journey.Journey]
getActiveJourneyIds riderId = do
  activeJourneys <- QJourney.findAllActiveByRiderId riderId
  return activeJourneys

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
postMultimodalJourneyCancel (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  void $ JM.cancelRemainingLegs journeyId False journey.riderId
  JM.updateJourneyStatus journey Domain.Types.Journey.CANCELLED
  pure Kernel.Types.APISuccess.Success

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

postMultimodalJourneyLegSkip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Int ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegSkip (mbPersonId, _) journeyId legOrder = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  JM.skipLeg journeyId legOrder False personId
  pure Kernel.Types.APISuccess.Success

postMultimodalJourneyLegAddSkippedLeg ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Int ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegAddSkippedLeg (_, _) journeyId legOrder = do
  JM.addSkippedLeg journeyId legOrder
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
      return $
        ApiTypes.MultimodalUserPreferences
          { allowedTransitModes = convertedModes <> [DTrip.Taxi],
            journeyOptionsSortingType = Just DMP.MOST_RELEVANT,
            busTransitTypes = Just [Spec.ORDINARY, Spec.EXPRESS, Spec.SPECIAL],
            subwayTransitTypes = Just [Spec.FIRST_CLASS, Spec.SECOND_CLASS]
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
    Environment.Flow API.Types.UI.MultimodalConfirm.PublicTransportData
  )
getPublicTransportData (mbPersonId, merchantId) mbCity _mbConfigVersion = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbRequestCity <- maybe (pure Nothing) (CQMOC.findByMerchantIdAndCity merchantId) mbCity
  let merchantOperatingCityId = maybe person.merchantOperatingCityId (.id) mbRequestCity
  let vehicleTypes = [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes

  let mkResponse stations routes bppConfig = do
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
              rsm = [],
              ptcv = gtfsVersion
            }

  let fetchData bppConfig = do
        stations <- OTPRest.getStationsByGtfsId bppConfig
        routes <- OTPRest.getRoutesByGtfsId bppConfig
        mkResponse stations routes bppConfig

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
      (_, availableRoutesByTiers) <- JLU.findPossibleRoutes (Just availableServiceTiers) fromStopCode toStopCode arrivalTime integratedBPPConfig merchantId person.merchantOperatingCityId vehicleCategory (vehicleCategory /= Enums.SUBWAY)
      return $ ApiTypes.LegServiceTierOptionsResp {options = availableRoutesByTiers}
    _ -> return $ ApiTypes.LegServiceTierOptionsResp {options = []}

castTravelModeToVehicleCategory :: DTrip.MultimodalTravelMode -> Enums.VehicleCategory
castTravelModeToVehicleCategory DTrip.Bus = Enums.BUS
castTravelModeToVehicleCategory DTrip.Taxi = Enums.AUTO_RICKSHAW
castTravelModeToVehicleCategory DTrip.Walk = Enums.AUTO_RICKSHAW
castTravelModeToVehicleCategory DTrip.Metro = Enums.METRO
castTravelModeToVehicleCategory DTrip.Subway = Enums.SUBWAY

-- TODO :: For Backward compatibility, remove this post `postMultimodalOrderSublegSetStatusV2` release
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

  legs <- QJourneyLeg.getJourneyLegs journeyId

  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")

  markLegStatus (Just newStatus) Nothing journeyLeg (Just subLegOrder)

  -- refetch updated legs and journey
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey False False updatedLegStatus
  updatedJourney <- JM.getJourney journeyId
  generateJourneyStatusResponse updatedJourney updatedLegStatus

postMultimodalOrderSublegSetStatusV2 ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Int ->
  Kernel.Prelude.Int ->
  JMState.TrackingStatus ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalOrderSublegSetStatusV2 (_, _) journeyId legOrder subLegOrder trackingStatus = do
  journey <- JM.getJourney journeyId

  legs <- QJourneyLeg.getJourneyLegs journeyId

  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")

  markLegStatus Nothing (Just trackingStatus) journeyLeg (Just subLegOrder)

  -- refetch updated legs and journey
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey False False updatedLegStatus
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
markAllSubLegsCompleted :: DJourneyLeg.JourneyLeg -> Environment.Flow ()
markAllSubLegsCompleted journeyLeg = do
  let subLegOrders = map (\r -> fromMaybe 1 r.subLegOrder) journeyLeg.routeDetails
  case subLegOrders of
    [] -> markLegStatus (Just JL.Completed) (Just JMState.Finished) journeyLeg Nothing
    orders -> mapM_ (\subOrder -> markLegStatus (Just JL.Completed) (Just JMState.Finished) journeyLeg (Just subOrder)) orders

postMultimodalComplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalComplete (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- QJourneyLeg.getJourneyLegs journeyId
  JMTypes.checkIfAnyTaxiLegOngoing legs

  mapM_ (\leg -> markAllSubLegsCompleted leg) legs
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkTerminalJourneyStatus journey True False updatedLegStatus
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
postMultimodalOrderSoftCancel (_, merchantId) journeyId _ = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  legs <- QJourneyLeg.getJourneyLegs journeyId
  JMTypes.checkIfAnyTaxiLegOngoing legs

  ticketBookings <- mapMaybeM (QFRFSTicketBooking.findBySearchId . Id) (mapMaybe (.legSearchId) legs)
  when (null ticketBookings) $ throwError (InvalidRequest "No FRFS bookings found for this journey")
  unless (length ticketBookings == 1) $ throwError (InvalidRequest "Multiple FRFS bookings found for this journey")
  let ticketBooking = head ticketBookings
  merchantOperatingCity <- CQMOC.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show ticketBooking.merchantOperatingCityId)
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow ticketBooking.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (ticketBooking.status == DFRFSB.CONFIRMED) $ throwError (InvalidRequest "Cancellation during incorrect status")
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.SOFT_CANCEL ticketBooking
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
  legs <- QJourneyLeg.getJourneyLegs journeyId
  JMTypes.checkIfAnyTaxiLegOngoing legs

  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
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
postMultimodalOrderCancel (_, merchantId) journeyId legOrder = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  legs <- QJourneyLeg.getJourneyLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  ticketBooking <- maybe (pure Nothing) (QFRFSTicketBooking.findBySearchId . Id) journeyLeg.legSearchId >>= fromMaybeM (InvalidRequest "No FRFS booking found for the leg")
  merchantOperatingCity <- CQMOC.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (InvalidRequest $ "Invalid merchant operating city id" <> ticketBooking.merchantOperatingCityId.getId)
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow ticketBooking.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL ticketBooking
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
  journeyLeg <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let groupCode = JMTypes.mkJourneyLegGroupCode (Id journey.searchRequestId) journeyLeg.mode journeyLeg.fromStopDetails journeyLeg.toStopDetails
  journeyLegs <- QJourneyLeg.findByGroupCode groupCode
  let journeyLegsWithTransits = map (\leg -> (mkStationCodesFromRouteDetails leg.routeDetails, leg)) journeyLegs
      sortedJourneyLegsWithTransits = sortBy (\a b -> compare (fst a) (fst b)) journeyLegsWithTransits
      uniqueJourneyLegsWithTransits = nubBy (\a b -> fst a == fst b) sortedJourneyLegsWithTransits
  journeyLegsInfo <- mapMaybeM (createLegOption person . snd) uniqueJourneyLegsWithTransits
  return $ API.Types.UI.MultimodalConfirm.SimilarJourneyLegsResp {journeyLegsInfo}
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
            (_, tiers) <- JLU.findPossibleRoutes Nothing fromStopCode toStopCode arrivalTime integratedBPPConfig merchantId person.merchantOperatingCityId vehicleCategory True
            return $ listToMaybe tiers
          _ -> return Nothing
      return $
        leg.estimatedMinFare <&> \fare ->
          JLU.JourneyLegOption
            { viaPoints = changeOverPoints,
              arrivalTimes = maybe [] (.nextAvailableBuses) mbAvailableTier,
              availableRoutes = maybe [] (.availableRoutes) mbAvailableTier,
              fare,
              duration = leg.duration,
              distance = leg.distance,
              journeyLegId = leg.id
            }

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
