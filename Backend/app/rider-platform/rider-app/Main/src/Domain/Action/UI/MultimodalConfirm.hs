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
    getMultimodalJourneyStatus,
  )
where

import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
-- import Domain.Types.Estimate
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Trip as DTrip
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
import Storage.Queries.JourneyLeg as QJourneyLeg
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
    legs <- JM.getAllLegsInfo journeyId
    let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) legs
    let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) legs
    let mbCurrency = listToMaybe legs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
    return $
      ApiTypes.JourneyInfoResp
        { estimatedDuration = journey.estimatedDuration,
          estimatedMinFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMinFareAmount,
          estimatedMaxFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMaxFareAmount,
          estimatedDistance = journey.estimatedDistance,
          legs
        }
  where
    lockKey = "infoLock-" <> journeyId.getId

postMultimodalConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalConfirm (_, _) journeyId = do
  journey <- JM.getJourney journeyId
  void $ JM.startJourney journey.id
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
  let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) legs
  let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) legs
  let mbCurrency = listToMaybe legs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
  return $
    ApiTypes.JourneyInfoResp
      { estimatedDuration = journey.estimatedDuration,
        estimatedMinFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMinFareAmount,
        estimatedMaxFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMaxFareAmount,
        estimatedDistance = journey.estimatedDistance,
        legs
      }

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
  journeyLegInfo <- find (\leg -> leg.order == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let mbPricingId = Id <$> journeyLegInfo.pricingId
  let legSearchId = Id journeyLegInfo.searchId
  mbEstimate <- maybe (pure Nothing) QEstimate.findById mbPricingId
  QSearchRequest.updatePricingId legSearchId (Just req.estimateId.getId)

  whenJust mbEstimate $ \estimate -> do
    when (estimate.status `elem` [DEst.COMPLETED, DEst.CANCELLED, DEst.GOT_DRIVER_QUOTE, DEst.DRIVER_QUOTE_CANCELLED]) $
      throwError $ InvalidRequest "Can't switch vehicle if driver has already being assigned"
    when (estimate.status == DEst.DRIVER_QUOTE_REQUESTED) $ JLI.confirm journeyLegInfo{pricingId = Just req.estimateId.getId}
  updatedLegs <- JM.getAllLegsInfo journeyId
  generateResponse journey updatedLegs
  where
    generateResponse journey updatedLegs = do
      let estimatedMinFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMinFare <&> (.amount)) updatedLegs
      let estimatedMaxFareAmount = sum $ mapMaybe (\leg -> leg.estimatedMaxFare <&> (.amount)) updatedLegs
      let mbCurrency = listToMaybe updatedLegs >>= (\leg -> leg.estimatedMinFare <&> (.currency))
      return $
        ApiTypes.JourneyInfoResp
          { estimatedDuration = journey.estimatedDuration,
            estimatedMinFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMinFareAmount,
            estimatedMaxFare = mkPriceAPIEntity $ mkPrice mbCurrency estimatedMaxFareAmount,
            estimatedDistance = journey.estimatedDistance,
            legs = updatedLegs
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
  allLegsInfo <- JM.getAllLegsInfo journeyId
  remainingLegs <- JM.getRemainingLegs journeyId
  let journeyLegData = find (\leg -> leg.sequenceNumber == req.legOrder) journeyLegs
  let legInfo = find (\leg -> leg.order == req.legOrder) allLegsInfo
  _ <- case legInfo of
    Just legData -> do
      case journeyLegData of
        Just journeyLeg -> do
          canSwitch <- JM.canBeSwitched legData req.newMode
          isCancellable <- JM.checkIfCancellable legData -- TODO : to be change
          if isCancellable && canSwitch
            then do
              isExtendLeg <- JM.isExtendable remainingLegs legData req.newMode
              if not isExtendLeg
                then do
                  JM.cancelLeg legData ""
                  newJourneyLeg <- JM.createJourneyLegFromCancelledLeg journeyLeg req.newMode
                  journey <- QJourney.findByPrimaryKey journeyId >>= fromMaybeM (InternalError ("Journey not found" <> show journeyId))
                  parentSearchReq <- QSearchRequest.findById (journey.searchRequestId) >>= fromMaybeM (SearchRequestNotFound legData.searchId)
                  QJourneyLeg.create newJourneyLeg
                  case req.newMode of
                    DTrip.Walk -> do
                      searchResp <- JM.addWalkLeg parentSearchReq newJourneyLeg req.originAddress req.destinationAddress
                      QJourneyLeg.updateLegSearchId (Just searchResp.id) newJourneyLeg.id
                      return searchResp
                    DTrip.Taxi -> do
                      searchResp <- JM.addTaxiLeg parentSearchReq newJourneyLeg req.originAddress req.destinationAddress
                      QJourneyLeg.updateLegSearchId (Just searchResp.id) newJourneyLeg.id
                      now <- getCurrentTime
                      let fiveMinutes = secondsToNominalDiffTime 300
                      when (diffUTCTime legData.startTime now <= fiveMinutes && legData.status /= JL.InPlan) $ do
                        void $ JLI.confirm legData
                      return searchResp
                    _ -> throwError $ InvalidRequest ("Mode not supported: " <> show req.newMode)
                else throwError $ InvalidRequest "Call the Extend Leg" ------------------------ TODO : Call the Extend Leg API ---------------------------
            else throwError $ InvalidRequest "Leg can not be Cancelled or Switched"
        Nothing -> throwError $ InvalidRequest "Leg not Present"
    Nothing -> throwError $ InvalidRequest "Leg not Present"
  pure Kernel.Types.APISuccess.Success

postMultimodalRiderLocation ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.RiderLocationReq ->
  Environment.Flow ApiTypes.JourneyStatus
postMultimodalRiderLocation personOrMerchantId journeyId req = do
  addPoint journeyId req
  getMultimodalJourneyStatus personOrMerchantId journeyId

postMultimodalJourneyCancel ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyCancel (_, _) journeyId = do
  _ <- JM.cancelRemainingLegs journeyId
  pure Kernel.Types.APISuccess.Success

postMultimodalExtendLeg ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  ApiTypes.ExtendLegReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalExtendLeg = do error "Logic yet to be decided"

postMultimodalJourneyLegSkip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegSkip (_, _) journeyId journeyLegId = do
  _ <- JM.skipLeg journeyId journeyLegId
  pure Kernel.Types.APISuccess.Success

getMultimodalJourneyStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatus
getMultimodalJourneyStatus (_, _) journeyId = do
  legs <- JM.getAllLegsStatus journeyId
  return $ ApiTypes.JourneyStatus {legs = map transformLeg legs}
  where
    transformLeg :: JMTypes.JourneyLegState -> ApiTypes.LegStatus
    transformLeg legState =
      ApiTypes.LegStatus
        { legOrder = legState.legOrder,
          status = legState.status,
          userPosition = legState.userPosition,
          vehiclePosition = legState.vehiclePosition
        }
