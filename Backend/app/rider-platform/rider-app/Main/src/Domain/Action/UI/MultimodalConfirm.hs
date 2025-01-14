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
import EulerHS.Prelude hiding (all, find, forM_, id, map)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyModule.Base
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.Types as JMTypes
import Storage.Queries.Estimate as QEstimate
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Storage.Queries.Journey as QJourney
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
    return $
      ApiTypes.JourneyInfoResp
        { estimatedDuration = journey.estimatedDuration,
          estimatedFare = mkPriceAPIEntity <$> journey.estimatedFare,
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
  return $
    ApiTypes.JourneyInfoResp
      { estimatedDuration = journey.estimatedDuration,
        estimatedFare = mkPriceAPIEntity <$> journey.estimatedFare,
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
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM (InvalidRequest "SearchRequest not found")
  journeyLegInfo <- searchRequest.journeyLegInfo & fromMaybeM (InvalidRequest "Journey Leg for SearchRequest not found")
  oldEstimateId <- journeyLegInfo.pricingId & fromMaybeM (InternalError "Old estimate id not found for search request")
  oldEstimate <- QEstimate.findById (Id oldEstimateId) >>= fromMaybeM (InternalError "Old estimate not found for search request")
  newEstimate <- QEstimate.findById estimateId >>= fromMaybeM (InvalidRequest "New Estimate requested not found")
  QSearchRequest.updatePricingId searchRequestId (Just estimateId.getId)
  let journeyId = journeyLegInfo.journeyId
  journey <- QJourney.findByPrimaryKey (Id journeyId) >>= fromMaybeM (InvalidRequest "Journey not found")
  initialFare <- journey.estimatedFare & fromMaybeM (InvalidRequest "Journey for SearchRequest not found")
  price1 <- initialFare `subtractPrice` oldEstimate.estimatedTotalFare
  newEstimatedPrice <- price1 `addPrice` newEstimate.estimatedTotalFare
  QJourney.updateEstimatedFare (Just newEstimatedPrice) (Id journeyId)
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
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalRiderLocation = do error "Logic yet to be decided"

postMultimodalJourneyCancel ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postMultimodalJourneyCancel = do error "Logic yet to be decided"

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
postMultimodalJourneyLegSkip = do error "Logic yet to be decided"

getMultimodalJourneyStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow [ApiTypes.LegStatus]
getMultimodalJourneyStatus (_, _) journeyId = do
  legs <- JM.getAllLegsStatus journeyId
  return $ map transformLeg legs
  where
    transformLeg :: JMTypes.JourneyLegState -> ApiTypes.LegStatus
    transformLeg legState =
      ApiTypes.LegStatus
        { legOrder = legState.legOrder,
          status = legState.status
        }
