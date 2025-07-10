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
    postMultimodalTicketVerify,
    postMultimodalComplete,
  )
where

import API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm
import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified API.UI.Select as Select
import BecknV2.FRFS.Enums
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FRFSTicketBooking as DFRFSB
import Domain.Types.FRFSTicketBookingPayment (FRFSTicketBookingPayment (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyFeedback as JFB
import qualified Domain.Types.JourneyLegsFeedbacks as JLFB
import qualified Domain.Types.Merchant
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (all, concatMap, elem, find, forM_, id, map, mapM_, sum, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.External.Encryption (decrypt)
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Payment.Interface.Types as KT
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import Kernel.Prelude hiding (foldl')
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Interface as JLI
import qualified Lib.JourneyLeg.Types as JL
import Lib.JourneyModule.Base
import qualified Lib.JourneyModule.Base as JM
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Lib.JourneyModule.Utils as JLU
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified SharedLogic.CreateFareForMultiModal as SMMFRFS
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuote as QQuote
import Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Storage.Queries.Journey as QJourney
import Storage.Queries.JourneyFeedback as SQJFB
import qualified Storage.Queries.JourneyLegsFeedbacks as SQJLFB
import Storage.Queries.JourneyRouteDetails as QJourneyRouteDetails
import Storage.Queries.MultimodalPreferences as QMP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderConfig as QRiderConfig
import Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.MultiModal as MM
import qualified Tools.Payment as TPayment

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
    legs <- JM.getAllLegsInfo journeyId False
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
postMultimodalConfirm (_, _) journeyId forcedBookLegOrder journeyConfirmReq = do
  journey <- JM.getJourney journeyId
  let confirmElements = journeyConfirmReq.journeyConfirmReqElements
  forM_ confirmElements $ \element -> do
    when element.skipBooking $ JM.skipLeg journeyId element.journeyLegOrder True
  void $ JM.startJourney confirmElements forcedBookLegOrder journey.id
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
getMultimodalBookingInfo (_personId, _merchantId) journeyId = do
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsInfo journeyId False
  JM.updateJourneyStatus journey Domain.Types.Journey.INPROGRESS -- fix it properly
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
  allJourneyFrfsBookings <- QFRFSTicketBooking.findAllByJourneyId (Just journeyId)
  frfsBookingStatusArr <- mapM (FRFSTicketService.frfsBookingStatus (personId, merchantId) True) allJourneyFrfsBookings
  let anyFirstBooking = listToMaybe frfsBookingStatusArr
      paymentOrder =
        anyFirstBooking >>= (.payment)
          <&> ( \p ->
                  ApiTypes.PaymentOrder {sdkPayload = p.paymentOrder, status = p.status}
              )
      allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyFrfsBookings
  journey <- JM.getJourney journeyId
  let paymentFareUpdate =
        catMaybes $
          map
            ( \booking ->
                if fromMaybe False booking.isFareChanged
                  then case booking.journeyLegOrder of
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
  when (journey.isPaymentSuccess /= Just True) $ do
    let mbPaymentStatus = paymentOrder <&> (.status)
    whenJust mbPaymentStatus $ \pstatus -> when (pstatus == FRFSTicketService.SUCCESS) $ void $ QJourney.updatePaymentStatus (Just True) journeyId
  -- handle if on_init doesn't come for all bookings once ui designs are there
  if allLegsOnInitDone
    then do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder,
            paymentFareUpdate = Just paymentFareUpdate
          }
    else do
      return $
        ApiTypes.JourneyBookingPaymentStatus
          { journeyId,
            paymentOrder = Nothing,
            paymentFareUpdate = Nothing
          }

postMultimodalPaymentUpdateOrder ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.UpdatePaymentOrderReq ->
    Environment.Flow API.Types.UI.MultimodalConfirm.UpdatePaymentOrderResp
  )
postMultimodalPaymentUpdateOrder (mbPersonId, _merchantId) journeyId req = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  frfsBookingsArr <- QFRFSTicketBooking.findAllByJourneyIdCond (Just journeyId)
  frfsBookingWithUpdatedPriceAndQty <-
    mapM
      ( \ticketBooking -> do
          quote <- QQuote.findById ticketBooking.quoteId >>= fromMaybeM (FRFSQuoteNotFound $ show ticketBooking.quoteId)
          let quantityRational = fromIntegral req.quantity :: Rational
          let oldQuantityRational = fromIntegral ticketBooking.quantity :: Rational
          let childTicketQuantityRational = fromIntegral req.childTicketQuantity :: Rational
          let childPrice = maybe (getHighPrecMoney quote.price.amount) (\p -> getHighPrecMoney p.amount) quote.childPrice
          let amountToBeUpdated = ((safeDiv (getHighPrecMoney ticketBooking.price.amount) oldQuantityRational) * quantityRational) + (childPrice * childTicketQuantityRational)
          let totalPrice =
                Price
                  { amount = HighPrecMoney $ amountToBeUpdated,
                    amountInt = Money $ roundToIntegral amountToBeUpdated,
                    currency = ticketBooking.price.currency
                  }
          let fRFSTicketBooking =
                ticketBooking
                  { DFRFSB.quantity = req.quantity,
                    DFRFSB.childTicketQuantity = Just req.childTicketQuantity,
                    DFRFSB.finalPrice = Just totalPrice,
                    DFRFSB.estimatedPrice = totalPrice,
                    DFRFSB.price = totalPrice
                  }
          QFRFSTicketBooking.updateByPrimaryKey fRFSTicketBooking
          pure $ ticketBooking{price = totalPrice, quantity = req.quantity}
      )
      frfsBookingsArr
  frfsBookingsPaymentArr <- mapM (QFRFSTicketBookingPayment.findAllTicketBookingId . (.id)) frfsBookingWithUpdatedPriceAndQty
  let paymentType = TPayment.FRFSMultiModalBooking
  (_vendorSplitDetails, amountUpdated) <- SMMFRFS.createVendorSplitFromBookings frfsBookingWithUpdatedPriceAndQty _merchantId person.merchantOperatingCityId paymentType
  isSplitEnabled <- TPayment.getIsSplitEnabled _merchantId person.merchantOperatingCityId Nothing paymentType
  let splitDetails = TPayment.mkUnaggregatedSplitSettlementDetails isSplitEnabled amountUpdated _vendorSplitDetails
  let flattenedPayments = concat frfsBookingsPaymentArr
  let mbPaymentOrderId = paymentOrderId <$> listToMaybe flattenedPayments
  case mbPaymentOrderId of
    Nothing ->
      return $
        ApiTypes.UpdatePaymentOrderResp
          { sdkPayload = Nothing
          }
    Just paymentOrderId -> do
      order <- QOrder.findById paymentOrderId >>= fromMaybeM (PaymentOrderNotFound paymentOrderId.getId)
      let updateReq =
            KT.OrderUpdateReq
              { amount = amountUpdated,
                orderShortId = order.shortId.getShortId,
                splitSettlementDetails = splitDetails
              }
      _ <- TPayment.updateOrder person.merchantId person.merchantOperatingCityId Nothing TPayment.FRFSMultiModalBooking (Just person.id.getId) person.clientSdkVersion updateReq
      QOrder.updateAmount order.id amountUpdated
      let updatedOrder :: DOrder.PaymentOrder
          updatedOrder = order {DOrder.amount = amountUpdated}
      sdkPayload <- buildUpdateOrderSDKPayload amountUpdated updatedOrder
      return $
        ApiTypes.UpdatePaymentOrderResp
          { sdkPayload = sdkPayload
          }
  where
    safeDiv :: (Eq a, Fractional a) => a -> a -> a
    safeDiv x 0 = x
    safeDiv x y = x / y

buildUpdateOrderSDKPayload :: EncFlow m r => HighPrecMoney -> DOrder.PaymentOrder -> m (Maybe Juspay.SDKPayloadDetails)
buildUpdateOrderSDKPayload amount order = do
  case (order.clientAuthToken, order.clientAuthTokenExpiry) of
    (Just token, Just clientAuthTokenExpiry) -> do
      clientAuthToken <- decrypt token
      return $
        Just
          Juspay.SDKPayloadDetails
            { clientId = order.clientId,
              amount = show amount,
              merchantId = order.paymentMerchantId,
              clientAuthToken,
              clientAuthTokenExpiry = clientAuthTokenExpiry,
              environment = order.environment,
              options_getUpiDeepLinks = order.getUpiDeepLinksOption,
              lastName = Nothing,
              action = Just "updateOrder",
              customerId = Just order.personId.getId,
              returnUrl = order.returnUrl,
              currency = order.currency,
              firstName = Nothing,
              customerPhone = Nothing,
              customerEmail = Nothing,
              orderId = Just order.shortId.getShortId,
              description = order.description,
              createMandate = order.createMandate,
              mandateMaxAmount = show <$> order.mandateMaxAmount,
              mandateStartDate = show . utcTimeToPOSIXSeconds <$> (order.mandateStartDate),
              mandateEndDate = show . utcTimeToPOSIXSeconds <$> order.mandateEndDate
            }
    (_, _) -> return Nothing

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
  legs <- JM.getAllLegsInfo journeyId False
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
    when (estimate.status == DEst.DRIVER_QUOTE_REQUESTED) $ do
      cancelPrevSearch legSearchId estimate.id
      JLI.confirm True Nothing Nothing journeyLegInfo{pricingId = Just req.estimateId.getId} Nothing
  updatedLegs <- JM.getAllLegsInfo journeyId False
  generateJourneyInfoResponse journey updatedLegs
  where
    cancelPrevSearch legSearchId estimateId = do
      searchReq <- QSearchRequest.findById legSearchId >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legSearchId.getId)
      cancelResponse <- Select.cancelSearch' (searchReq.riderId, searchReq.merchantId) estimateId
      case cancelResponse of
        Select.Success -> return ()
        Select.BookingAlreadyCreated -> throwError (InternalError $ "Cannot cancel search as booking is already created for searchId: " <> show legSearchId.getId)
        Select.FailedToCancel -> throwError (InvalidRequest $ "Failed to cancel search for searchId: " <> show legSearchId.getId)

updateFRFSBookingAndPayment ::
  Domain.Types.Person.Person ->
  DFRFSB.FRFSTicketBooking ->
  HighPrecMoney ->
  FRFSTicketBookingPayment ->
  Environment.Flow ()
updateFRFSBookingAndPayment person booking totalPrice payment = do
  QFRFSTicketBooking.updateByPrimaryKey booking
  order <- QOrder.findById payment.paymentOrderId >>= fromMaybeM (PaymentOrderNotFound payment.paymentOrderId.getId)
  let updateReq = KT.OrderUpdateReq {amount = totalPrice, orderShortId = order.shortId.getShortId, splitSettlementDetails = Nothing}
  _ <- TPayment.updateOrder person.merchantId person.merchantOperatingCityId Nothing TPayment.FRFSMultiModalBooking (Just person.id.getId) person.clientSdkVersion updateReq
  QOrder.updateAmount order.id totalPrice

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
  legs <- JM.getAllLegsInfo journeyId False
  journeyLegInfo <- find (\leg -> leg.order == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  QFRFSSearch.updatePricingId (Id journeyLegInfo.searchId) (Just req.quoteId.getId)
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  allJourneyFrfsBookings <- QFRFSTicketBooking.findAllByJourneyId (Just journeyId)
  let mbBooking = find (\booking -> booking.searchId == Id journeyLegInfo.searchId) allJourneyFrfsBookings
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
        updatedTotalPrice =
          foldl'
            ( \acc booking' ->
                if booking'.id == booking.id
                  then acc + totalPriceForSwitchLeg.amount
                  else acc + booking'.price.amount
            )
            (HighPrecMoney 0)
            allJourneyFrfsBookings
    payments <- QFRFSTicketBookingPayment.findAllTicketBookingId booking.id
    whenJust (listToMaybe payments) $ \payment -> updateFRFSBookingAndPayment person updatedBooking updatedTotalPrice payment
  alternateShortNames <- getAlternateShortNames
  QJourneyRouteDetails.updateAlternateShortNames alternateShortNames (Id journeyLegInfo.searchId)
  updatedLegs <- JM.getAllLegsInfo journeyId False
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
      void $ JM.startJourney [] (Just nextLeg.legOrder) journeyId
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
  JM.skipLeg journeyId legOrder False
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
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsStatus journey
  generateJourneyStatusResponse personId merchantId journey legs

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
    mkFeedbackFormData feedBackForjourney ratingForLegs =
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
    forM
      vehicleTypes
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vType DIBC.MULTIMODAL
      )

  let mkResponse stations routes bppConfig =
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
              ptcv = bppConfig.feedKey
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
          (concat integratedBPPConfigs)
      )
      >>= \case
        Left _ -> mapM fetchData (concat integratedBPPConfigs)
        Right configsWithFeedInfo -> do
          -- Group configs by feed_id and take first config for each feed_id
          let configsByFeedId = HashMap.fromListWith (++) $ map (\(config, (feedKey, _)) -> (feedKey, [config])) configsWithFeedInfo
              uniqueConfigs = map (head . snd) $ HashMap.toList configsByFeedId
          mapM fetchData uniqueConfigs
  let transportData =
        ApiTypes.PublicTransportData
          { ss = concatMap (.ss) transportDataList,
            rs = concatMap (.rs) transportDataList,
            rsm = concatMap (.rsm) transportDataList,
            ptcv = T.intercalate (T.pack "#") $ map (.feedKey) (concat integratedBPPConfigs)
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
  legs <- JM.getJourneyLegs journeyId
  now <- getCurrentTime
  journeyLegInfo <- find (\leg -> leg.sequenceNumber == legOrder) legs & fromMaybeM (InvalidRequest "No matching journey leg found for the given legOrder")
  let mbAgencyId = journeyLegInfo.agency >>= (.gtfsId)
  let mbRouteDetail = journeyLegInfo.routeDetails & listToMaybe
  let mbFomStopCode = mbRouteDetail >>= (.fromStopDetails) >>= (.stopCode)
  let mbToStopCode = mbRouteDetail >>= (.toStopDetails) >>= (.stopCode)
  let vehicleCategory = castTravelModeToVehicleCategory journeyLegInfo.mode
  let mbArrivalTime = mbRouteDetail >>= (.fromArrivalTime)
  let arrivalTime = fromMaybe now mbArrivalTime
  mbIntegratedBPPConfig <- SIBC.findMaybeIntegratedBPPConfigFromAgency mbAgencyId person.merchantOperatingCityId vehicleCategory DIBC.MULTIMODAL
  case (mbFomStopCode, mbToStopCode, mbIntegratedBPPConfig) of
    (Just fromStopCode, Just toStopCode, Just integratedBPPConfig) -> do
      quotes <- maybe (pure []) (QFRFSQuote.findAllBySearchId . Id) journeyLegInfo.legSearchId
      let availableServiceTiers = mapMaybe JMTypes.getServiceTierFromQuote quotes
      (_, availableRoutesByTier) <- JLU.findPossibleRoutes (Just availableServiceTiers) fromStopCode toStopCode arrivalTime integratedBPPConfig merchantId person.merchantOperatingCityId vehicleCategory
      return $ ApiTypes.LegServiceTierOptionsResp {options = availableRoutesByTier}
    _ -> return $ ApiTypes.LegServiceTierOptionsResp {options = []}
  where
    castTravelModeToVehicleCategory :: DTrip.MultimodalTravelMode -> Enums.VehicleCategory
    castTravelModeToVehicleCategory DTrip.Bus = Enums.BUS
    castTravelModeToVehicleCategory DTrip.Taxi = Enums.AUTO_RICKSHAW
    castTravelModeToVehicleCategory DTrip.Walk = Enums.AUTO_RICKSHAW
    castTravelModeToVehicleCategory DTrip.Metro = Enums.METRO
    castTravelModeToVehicleCategory DTrip.Subway = Enums.SUBWAY

postMultimodalOrderSublegSetStatus ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Kernel.Prelude.Int ->
  Kernel.Prelude.Int ->
  JL.JourneyLegStatus ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalOrderSublegSetStatus (mbPersonId, merchantId) journeyId legOrder subLegOrder newStatus = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  journey <- JM.getJourney journeyId

  -- Removed the 'unless' condition to allow updates for JL.Completed and JL.Cancelled statuses
  legs <- JM.getAllLegsInfo journeyId False
  journeyLegInfo <- find (\leg -> leg.order == legOrder) legs & fromMaybeM (InvalidRequest $ "No matching journey leg found for the given legOrder")

  markLegStatus newStatus journeyLegInfo.legExtraInfo (Just subLegOrder)

  -- refetch updated legs and journey
  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkJourneyAsFeedbackPending journey updatedLegStatus
  updatedJourney <- JM.getJourney journeyId
  generateJourneyStatusResponse personId merchantId updatedJourney updatedLegStatus

postMultimodalTicketVerify ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.UI.MultimodalConfirm.MultimodalTicketVerifyReq ->
  Environment.Flow API.Types.UI.MultimodalConfirm.MultimodalTicketVerifyResp
postMultimodalTicketVerify (_mbPersonId, merchantId) opCity req = do
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchantId) (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory BUS) >>= fromMaybeM (InternalError "Beckn Config not found")
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show opCity)
  let verifyTicketsAndBuildResponse provider tickets = do
        legInfoList <- forM tickets $ \ticketQR -> do
          ticket <- CallExternalBPP.verifyTicket merchantId merchantOperatingCity bapConfig BUS ticketQR DIBC.MULTIMODAL
          booking <- QFRFSTicketBooking.findById ticket.frfsTicketBookingId >>= fromMaybeM (BookingNotFound ticket.frfsTicketBookingId.getId)
          JMTypes.mkLegInfoFromFrfsBooking booking Nothing Nothing Nothing Nothing
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

-- Helper function to get sub-leg orders from Metro/Subway leg extra info
getSubLegOrders :: JMTypes.LegExtraInfo -> [Int]
getSubLegOrders legExtraInfo =
  case legExtraInfo of
    JMTypes.Metro metroInfo -> mapMaybe (.subOrder) metroInfo.routeInfo
    JMTypes.Subway subwayInfo -> mapMaybe (.subOrder) subwayInfo.routeInfo
    _ -> []

-- Helper function to mark all sub-legs as completed for FRFS legs
markAllSubLegsCompleted :: JMTypes.LegExtraInfo -> Environment.Flow ()
markAllSubLegsCompleted legExtraInfo = do
  let subLegOrders = getSubLegOrders legExtraInfo
  case subLegOrders of
    [] -> markLegStatus JL.Completed legExtraInfo Nothing
    orders -> mapM_ (\subOrder -> markLegStatus JL.Completed legExtraInfo (Just subOrder)) orders

postMultimodalComplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
  Environment.Flow ApiTypes.JourneyStatusResp
postMultimodalComplete (mbPersonId, merchantId) journeyId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  journey <- JM.getJourney journeyId
  legs <- JM.getAllLegsInfo journeyId False
  mapM_ (\leg -> when (leg.travelMode == DTrip.Taxi) $ JM.cancelLeg leg (SCR.CancellationReasonCode "") False False) legs
  mapM_ (markAllSubLegsCompleted . (.legExtraInfo)) legs

  updatedLegStatus <- JM.getAllLegsStatus journey
  checkAndMarkJourneyAsFeedbackPending journey updatedLegStatus
  updatedJourney <- JM.getJourney journeyId
  generateJourneyStatusResponse personId merchantId updatedJourney updatedLegStatus
