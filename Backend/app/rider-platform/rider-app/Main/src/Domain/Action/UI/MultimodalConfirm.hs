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
  )
where

import API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyFeedback as JFB
import qualified Domain.Types.JourneyLegsFeedbacks as JLFB
import qualified Domain.Types.Merchant
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (all, concatMap, elem, find, forM_, id, map, mapM_, sum, whenJust)
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
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
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Estimate as QEstimate
import Storage.Queries.FRFSSearch as QFRFSSearch
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Storage.Queries.Journey as QJourney
import Storage.Queries.JourneyFeedback as SQJFB
import qualified Storage.Queries.JourneyLegsFeedbacks as SQJLFB
import Storage.Queries.MultimodalPreferences as QMP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderConfig as QRiderConfig
import qualified Storage.Queries.Route as QRoute
import Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.Station as QStation
import Tools.Error

-- import  Domain.Types.Location as DTL (Location(..))

postMultimodalInitiate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
postMultimodalInitiate (_personId, _merchantId) journeyId = do
  Redis.withLockRedisAndReturnValue lockKey 60 $ do
    journeyLegs <- getJourneyLegs journeyId
    addAllLegs journeyId journeyLegs
    journey <- JM.getJourney journeyId
    JM.updateJourneyStatus journey Domain.Types.Journey.INITIATED
    legs <- JM.getAllLegsInfo journeyId
    now <- getCurrentTime
    generateJourneyInfoResponse journey legs now
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
  let ticketQuantityMap = Map.fromList [(element.journeyLegOrder, fromMaybe 1 element.ticketQuantity) | element <- confirmElements]
  forM_ confirmElements $ \element ->
    when element.skipBooking $
      JM.skipLeg journeyId element.journeyLegOrder
  void $ JM.startJourney forcedBookLegOrder (Just ticketQuantityMap) journey.id
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
  generateJourneyInfoResponse journey legs now

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
  -- handle if on_init doesn't come for all bookings once ui designs are there
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
  unless (journeyLegInfo.travelMode == DTrip.Taxi) $
    throwError (JourneyLegCannotBeCancelled (show journeyLegInfo.order))
  when (journeyLegInfo.status `elem` JMTypes.cannotSwitchStatus) $
    throwError (JourneyLegCannotBeCancelled (show journeyLegInfo.order))
  let mbPricingId = Id <$> journeyLegInfo.pricingId
  let legSearchId = Id journeyLegInfo.searchId
  mbEstimate <- maybe (pure Nothing) QEstimate.findById mbPricingId
  QSearchRequest.updatePricingId legSearchId (Just req.estimateId.getId)

  whenJust mbEstimate $ \estimate -> do
    when (estimate.status `elem` [DEst.COMPLETED, DEst.CANCELLED, DEst.GOT_DRIVER_QUOTE, DEst.DRIVER_QUOTE_CANCELLED]) $
      throwError $ InvalidRequest "Can't switch vehicle if driver has already being assigned"
    when (estimate.status == DEst.DRIVER_QUOTE_REQUESTED) $ JLI.confirm True Nothing journeyLegInfo{pricingId = Just req.estimateId.getId}
  updatedLegs <- JM.getAllLegsInfo journeyId
  generateJourneyInfoResponse journey updatedLegs now

postMultimodalOrderSwitchFRFSTier ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.SwitchFRFSTierReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSwitchFRFSTier (_, _) journeyId legOrder req = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsInfo journeyId
  now <- getCurrentTime
  journeyLegInfo <- find (\leg -> leg.order == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  QFRFSSearch.updatePricingId (Id journeyLegInfo.searchId) (Just req.quoteId.getId)
  updatedLegs <- JM.getAllLegsInfo journeyId
  generateJourneyInfoResponse journey updatedLegs now

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

generateJourneyInfoResponse :: (CacheFlow m r, EsqDBFlow m r) => Domain.Types.Journey.Journey -> [JMTypes.LegInfo] -> UTCTime -> m ApiTypes.JourneyInfoResp
generateJourneyInfoResponse journey legs now = do
  let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) legs
  let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) legs
  let unifiedQR = getUnifiedQR legs now
  let mbCurrency = listToMaybe legs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
  merchantOperatingCity <- maybe (pure Nothing) CQMOC.findById journey.merchantOperatingCityId
  let merchantOperatingCityName = show . (.city) <$> merchantOperatingCity

  pure $
    ApiTypes.JourneyInfoResp
      { estimatedDuration = journey.estimatedDuration,
        estimatedMinFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMinFareAmount,
        estimatedMaxFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMaxFareAmount,
        estimatedDistance = journey.estimatedDistance,
        journeyStatus = journey.status,
        legs,
        unifiedQR,
        startTime = journey.startTime,
        endTime = journey.endTime,
        merchantOperatingCityName
      }

postMultimodalSwitch ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.SwitchLegReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalSwitch (_, _) journeyId req = do
  JM.switchLeg journeyId req
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
      void $ JM.startJourney (Just nextLeg.legOrder) Nothing journeyId
  return journeyStatus

postMultimodalJourneyCancel ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyCancel (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  void $ JM.cancelRemainingLegs journeyId False
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
  return $ ApiTypes.JourneyStatusResp {legs = concat (map transformLeg legs), journeyStatus = journey.status, journeyPaymentStatus = paymentStatus}
  where
    transformLeg :: JMTypes.JourneyLegState -> [ApiTypes.LegStatus]
    transformLeg legState =
      case legState of
        JMTypes.Single legData -> [convert legData]
        JMTypes.Transit legDataList -> map convert legDataList
      where
        convert legData =
          ApiTypes.LegStatus
            { legOrder = legData.legOrder,
              subLegOrder = legData.subLegOrder,
              status = legData.status,
              userPosition = legData.userPosition,
              vehiclePosition = legData.vehiclePosition,
              mode = legData.mode,
              nextStop = legData.nextStop,
              nextStopTravelTime = legData.nextStopTravelTime,
              nextStopTravelDistance = legData.nextStopTravelDistance
            }

postMultimodalJourneyFeedback :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> API.Types.UI.MultimodalConfirm.JourneyFeedBackForm -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyFeedback (mbPersonId, mbMerchantId) journeyId journeyFeedbackForm = do
  journey <- JM.getJourney journeyId
  riderId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  ratingForLegs <- SQJLFB.findAllByJourneyId journeyId
  whenJust journeyFeedbackForm.rating $ \rating -> do
    let mkJourneyfeedbackForm =
          JFB.JourneyFeedback
            { additionalFeedBack = journeyFeedbackForm.additionalFeedBack,
              journeyId = journeyId,
              rating = Just rating,
              riderId = riderId,
              merchantId = Just mbMerchantId,
              merchantOperatingCityId = journey.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
    SQJFB.create mkJourneyfeedbackForm
    JM.updateJourneyStatus journey Domain.Types.Journey.COMPLETED

  let mkJourneyLegsFeedback feedbackEntry =
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
  mapM_ (findAndUpdate ratingForLegs) $ map mkJourneyLegsFeedback journeyFeedbackForm.rateTravelMode
  pure Kernel.Types.APISuccess.Success
  where
    findAndUpdate :: [JLFB.JourneyLegsFeedbacks] -> JLFB.JourneyLegsFeedbacks -> Environment.Flow ()
    findAndUpdate [] legRating = SQJLFB.create legRating
    findAndUpdate (ratingForLegs : nextRatingForLegs) legRating =
      if ratingForLegs.legOrder == legRating.legOrder then SQJLFB.updateByPrimaryKey legRating else findAndUpdate nextRatingForLegs legRating

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
            journeyOptionsSortingType = Just multimodalUserPreferences'.journeyOptionsSortingType
          }
    Nothing -> do
      personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
      riderConfig <- QRiderConfig.findByMerchantOperatingCityId personCityInfo.merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound personCityInfo.merchantOperatingCityId.getId)
      let convertedModes = mapMaybe generalVehicleTypeToAllowedTransitMode (fromMaybe [] riderConfig.permissibleModes)
      return $
        ApiTypes.MultimodalUserPreferences
          { allowedTransitModes = convertedModes,
            journeyOptionsSortingType = Just DMP.FASTEST
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
    Just _ ->
      QMP.updateAllowedTransitModesAndJourneyOptionsSortingType updatedAllowedModes (fromMaybe DMP.FASTEST multimodalUserPreferences.journeyOptionsSortingType) personId
    Nothing -> do
      now <- getCurrentTime
      let newPreferences =
            MultimodalPreferences
              { allowedTransitModes = updatedAllowedModes,
                journeyOptionsSortingType = fromMaybe DMP.FASTEST multimodalUserPreferences.journeyOptionsSortingType,
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
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.MultimodalConfirm.PublicTransportData
  )
getPublicTransportData (mbPersonId, _merchantId) _mbConfigVersion = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let vehicleTypes = [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  integratedBPPConfigs <-
    catMaybes
      <$> forM
        vehicleTypes
        ( \vType ->
            QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) person.merchantOperatingCityId vType DIBC.MULTIMODAL
        )

  let fetchData bppConfig = do
        stations <- QStation.findAllByBppConfigId bppConfig.id
        routes <- QRoute.findAllByBppConfigId bppConfig.id
        -- routeStops <- QRouteStopMapping.findAllByBppConfigId bppConfig.id
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
                              vt = show s.vehicleType
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
                          vt = show r.vehicleType,
                          clr = r.color
                        }
                  )
                  routes,
              rsm = [],
              -- map
              --   ( \rs ->
              --       ApiTypes.TransportRouteStopMapping
              --         { rc = rs.routeCode,
              --           sc = rs.stopCode,
              --           sn = rs.sequenceNum
              --         }
              --   )
              --   routeStops,
              ptcv = bppConfig.id.getId
            }

  transportDataList <- mapM fetchData integratedBPPConfigs
  let transportData =
        ApiTypes.PublicTransportData
          { ss = concatMap (.ss) transportDataList,
            rs = concatMap (.rs) transportDataList,
            rsm = concatMap (.rsm) transportDataList,
            ptcv = T.intercalate (T.pack "#") $ map (.ptcv) transportDataList
          }
  return transportData
