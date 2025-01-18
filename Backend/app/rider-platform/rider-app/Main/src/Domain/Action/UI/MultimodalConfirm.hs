module Domain.Action.UI.MultimodalConfirm
  ( postMultimodalInfo,
    postMultimodalConfirm,
    getMultimodalBookingInfo,
    getMultimodalSwitchTaxi,
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
import Domain.Types.Estimate
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Domain.Types.SearchRequest
import Environment
import EulerHS.Prelude hiding (all, find, forM_, id, map, sum)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyModule.Base
import qualified Lib.JourneyModule.Base as JM
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JMTypes
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

postMultimodalInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    ApiTypes.JourneyInfoReq ->
    Environment.Flow ApiTypes.JourneyInfoResp
  )
postMultimodalInfo (_personId, _merchantId) journeyId req = do
  Redis.withLockRedisAndReturnValue lockKey 60 $ do
    addAllLegs journeyId req.legsReq
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

getMultimodalSwitchTaxi ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest ->
    Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
getMultimodalSwitchTaxi (_, _) searchRequestId estimateId = do
  QSearchRequest.updatePricingId searchRequestId (Just estimateId.getId)
  pure Kernel.Types.APISuccess.Success

postMultimodalSwitch ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Text ->
  ApiTypes.SwitchLegReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalSwitch = do error "Logic yet to be decided"

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
          status = legState.status
        }
