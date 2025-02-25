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
  )
where

import API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyFeedback as JFB
import qualified Domain.Types.JourneyLegsFeedbacks as JLFB
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (all, elem, find, forM_, id, map, sum, whenJust)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Interface as JLI
import qualified Lib.JourneyLeg.Types as JL
import Lib.JourneyModule.Base
import qualified Lib.JourneyModule.Base as JM
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Storage.Queries.Estimate as QEstimate
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Storage.Queries.Journey as QJourney
import Storage.Queries.JourneyFeedback as SQJFB
import Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.JourneyLegsFeedbacks as SQJLFB
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

postMultimodalInitiate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
postMultimodalInitiate (_personId, _merchantId) journeyId = do
  Redis.withLockRedisAndReturnValue lockKey 60 $ do
    addAllLegs journeyId
    journey <- JM.getJourney journeyId
    JM.updateJourneyStatus journey Domain.Types.Journey.INITIATED
    legs <- JM.getAllLegsInfo journeyId
    now <- getCurrentTime
    return $ generateJourneyInfoResponse journey legs now
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
postMultimodalConfirm (_, _) journeyId forcedBookLegOrder journeyConfirmReq = do
  journey <- JM.getJourney journeyId
  let confirmElements = journeyConfirmReq.journeyConfirmReqElements
  forM_ confirmElements $ \element ->
    when (element.skipBooking) $
      JM.skipLeg journeyId element.journeyLegOrder
  void $ JM.startJourney forcedBookLegOrder journey.id
  JM.updateJourneyStatus journey Domain.Types.Journey.CONFIRMED
  pure Kernel.Types.APISuccess.Success

getMultimodalBookingInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
getMultimodalBookingInfo (_personId, _merchantId) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsInfo journeyId
  now <- getCurrentTime
  JM.updateJourneyStatus journey Domain.Types.Journey.INPROGRESS -- fix it properly
  return $ generateJourneyInfoResponse journey legs now

getMultimodalBookingPaymentStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyBookingPaymentStatus
  )
getMultimodalBookingPaymentStatus (mbPersonId, merchantId) journeyId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  allJourneyFrfsBookings <- QFRFSTicketBooking.findAllByJourneyId (Just journeyId)
  let allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyFrfsBookings
  -- handle if onit doesn't come for all bookings once ui designs are there
  if allLegsOnInitDone
    then do
      frfsBookingStatusArr <- FRFSTicketService.frfsBookingStatus (personId, merchantId) `mapM` allJourneyFrfsBookings
      let anyFirstBooking = listToMaybe frfsBookingStatusArr -- take any first booking as payment is same for all
          paymentOrder = anyFirstBooking >>= (.payment) <&> (\p -> ApiTypes.PaymentOrder {sdkPayload = p.paymentOrder, status = p.status})
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder
          }
    else do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder = Nothing
          }

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
  legs <- JM.getAllLegsInfo journeyId
  now <- getCurrentTime
  journeyLegInfo <- find (\leg -> leg.order == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let mbPricingId = Id <$> journeyLegInfo.pricingId
  let legSearchId = Id journeyLegInfo.searchId
  mbEstimate <- maybe (pure Nothing) QEstimate.findById mbPricingId
  QSearchRequest.updatePricingId legSearchId (Just req.estimateId.getId)

  whenJust mbEstimate $ \estimate -> do
    when (estimate.status `elem` [DEst.COMPLETED, DEst.CANCELLED, DEst.GOT_DRIVER_QUOTE, DEst.DRIVER_QUOTE_CANCELLED]) $
      throwError $ InvalidRequest "Can't switch vehicle if driver has already being assigned"
    when (estimate.status == DEst.DRIVER_QUOTE_REQUESTED) $ JLI.confirm True journeyLegInfo{pricingId = Just req.estimateId.getId}
  updatedLegs <- JM.getAllLegsInfo journeyId
  return $ generateJourneyInfoResponse journey updatedLegs now

getActiveJourneyIds ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m [Kernel.Types.Id.Id Domain.Types.Journey.Journey]
getActiveJourneyIds riderId = do
  activeJourneys <- QJourney.findAllActiveByRiderId riderId
  return $ activeJourneys <&> (.id)

generateJourneyInfoResponse :: Domain.Types.Journey.Journey -> [JMTypes.LegInfo] -> UTCTime -> ApiTypes.JourneyInfoResp
generateJourneyInfoResponse journey legs now = do
  let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) legs
  let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) legs
  let unifiedQR = getUnifiedQR legs now
  let mbCurrency = listToMaybe legs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
  ApiTypes.JourneyInfoResp
    { estimatedDuration = journey.estimatedDuration,
      estimatedMinFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMinFareAmount,
      estimatedMaxFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMaxFareAmount,
      estimatedDistance = journey.estimatedDistance,
      journeyStatus = journey.status,
      legs,
      unifiedQR,
      startTime = journey.startTime,
      endTime = journey.endTime
    }

postMultimodalSwitch ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.SwitchLegReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalSwitch (_, _) journeyId req = do
  journeyLegs <- JM.getJourneyLegs journeyId
  remainingLegs <- JM.getRemainingLegs journeyId
  journeyLeg <- find (\leg -> leg.sequenceNumber == req.legOrder) journeyLegs & fromMaybeM (InvalidRequest ("Journey Leg not Present" <> show req.legOrder))
  legData <- find (\leg -> leg.order == req.legOrder) remainingLegs & fromMaybeM (InvalidRequest ("Journey Leg not Present" <> show req.legOrder))
  canSwitch <- JM.canBeSwitched legData req.newMode
  isCancellable <- JM.checkIfCancellable legData
  if isCancellable
    then do
      if canSwitch
        then do
          isExtendLeg <- JM.isExtendable remainingLegs legData req.newMode
          if not isExtendLeg
            then do
              JM.cancelLeg legData (SCR.CancellationReasonCode "") False
              newJourneyLeg <- JM.createJourneyLegFromCancelledLeg journeyLeg req.newMode req.startLocation
              QJourneyLeg.create newJourneyLeg
              addAllLegs journeyId
              when (legData.status /= JL.InPlan) $
                fork "Start journey thread" $ withShortRetry $ startJourney Nothing journeyId
            else throwError $ InvalidRequest "Call the Extend Leg" -- TODO : Call the Extend Leg API
        else throwError $ JourneyLegCannotBeSwitched journeyLeg.id.getId
    else throwError $ JourneyLegCannotBeCancelled journeyLeg.id.getId
  pure Kernel.Types.APISuccess.Success

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
  forM_ (zip journeyStatus.legs (drop 1 journeyStatus.legs)) $ \(currentLeg, nextLeg) -> do
    when ((currentLeg.status == JL.Finishing || currentLeg.status == JL.Completed) && nextLeg.status == JL.InPlan && nextLeg.mode == DTrip.Taxi) $
      void $ JM.startJourney (Just nextLeg.legOrder) journeyId
  return journeyStatus

postMultimodalJourneyCancel ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyCancel (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  void $ JM.cancelRemainingLegs journeyId
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
postMultimodalJourneyLegSkip (_, _) journeyId legOrder = do
  JM.skipLeg journeyId legOrder
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
getMultimodalJourneyStatus (mbPersonId, merchantId) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsStatus journey
  paymentStatus <-
    if journey.isPaymentSuccess /= Just True
      then do
        personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
        allJourneyFrfsBookings <- QFRFSTicketBooking.findAByJourneyIdCond (Just journeyId)
        frfsBookingStatusArr <- mapM (FRFSTicketService.frfsBookingStatus (personId, merchantId)) allJourneyFrfsBookings
        let anyFirstBooking = listToMaybe frfsBookingStatusArr
            paymentOrder =
              anyFirstBooking >>= (.payment)
                <&> ( \p ->
                        ApiTypes.PaymentOrder {sdkPayload = p.paymentOrder, status = p.status}
                    )
            mbPaymentStatus = paymentOrder <&> (.status)
        whenJust mbPaymentStatus $ \pstatus -> do
          when (pstatus == FRFSTicketService.SUCCESS) $ void $ QJourney.updatePaymentStatus (Just True) journeyId
        return $ paymentOrder <&> (.status)
      else
        if journey.isPaymentSuccess == Just True
          then do
            return (Just FRFSTicketService.SUCCESS)
          else return Nothing
  return $ ApiTypes.JourneyStatusResp {legs = map transformLeg legs, journeyStatus = journey.status, journeyPaymentStatus = paymentStatus}
  where
    transformLeg :: JMTypes.JourneyLegState -> ApiTypes.LegStatus
    transformLeg legState =
      ApiTypes.LegStatus
        { legOrder = legState.legOrder,
          status = legState.status,
          userPosition = legState.userPosition,
          vehiclePosition = legState.vehiclePosition,
          mode = legState.mode,
          nextStop = legState.nextStop,
          nextStopTravelTime = legState.nextStopTravelTime,
          nextStopTravelDistance = legState.nextStopTravelDistance
        }

postMultimodalJourneyFeedback :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> API.Types.UI.MultimodalConfirm.JourneyFeedBackForm -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyFeedback (mbPersonId, mbMerchantId) journeyId journeyFeedbackForm = do
  journey <- JM.getJourney journeyId
  riderId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  let mkJourneyfeedbackForm =
        JFB.JourneyFeedback
          { additionalFeedBack = journeyFeedbackForm.additionalFeedBack,
            journeyId = journeyId,
            rating = journeyFeedbackForm.rating,
            riderId = riderId,
            merchantId = Just mbMerchantId,
            merchantOperatingCityId = journey.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

      mkJourneyLegsFeedback feedbackEntry =
        JLFB.JourneyLegsFeedbacks
          { isExperienceGood = feedbackEntry.isExperienceGood,
            journeyId = journeyId,
            travelMode = feedbackEntry.travelMode,
            legOrder = feedbackEntry.legOrder,
            merchantId = Just mbMerchantId,
            merchantOperatingCityId = journey.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  SQJFB.create mkJourneyfeedbackForm
  SQJLFB.createMany $ map mkJourneyLegsFeedback journeyFeedbackForm.rateTravelMode
  JM.updateJourneyStatus journey Domain.Types.Journey.COMPLETED
  pure Kernel.Types.APISuccess.Success

getMultimodalFeedback :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Environment.Flow (Kernel.Prelude.Maybe ApiTypes.JourneyFeedBackForm)
getMultimodalFeedback (mbPersonId, _) journeyId = do
  _ <- JM.getJourney journeyId
  _ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  feedBackForjourney' <- SQJFB.findByJourneyId journeyId
  case feedBackForjourney' of
    Just feedBackForjourney -> do
      ratingForLegs' <- SQJLFB.findAllByJourneyId journeyId
      let ratingForLegs = map mkRatingForLegs ratingForLegs'
      return $ Just (mkFeedbackFormData feedBackForjourney ratingForLegs)
    Nothing -> return Nothing
  where
    mkFeedbackFormData :: JFB.JourneyFeedback -> [ApiTypes.RateMultiModelTravelModes] -> ApiTypes.JourneyFeedBackForm
    mkFeedbackFormData feedBackForjourney ratingForLegs = do
      ApiTypes.JourneyFeedBackForm
        { rating = feedBackForjourney.rating,
          additionalFeedBack = feedBackForjourney.additionalFeedBack,
          rateTravelMode = ratingForLegs
        }

    mkRatingForLegs :: JLFB.JourneyLegsFeedbacks -> ApiTypes.RateMultiModelTravelModes
    mkRatingForLegs ratingForLeg =
      ApiTypes.RateMultiModelTravelModes
        { isExperienceGood = ratingForLeg.isExperienceGood,
          legOrder = ratingForLeg.legOrder,
          travelMode = ratingForLeg.travelMode
        }
