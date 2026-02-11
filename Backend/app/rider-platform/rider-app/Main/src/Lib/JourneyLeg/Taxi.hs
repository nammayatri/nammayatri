{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Taxi where

import qualified API.UI.Select as DSelect
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Maybe ()
import Data.Ord (comparing)
import qualified Data.Text as T
import Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.Booking
import Domain.Types.BookingStatus
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Domain.Types.ServiceTierType ()
import Kernel.External.Maps.Types
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Taxi
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import qualified Lib.JourneyModule.Types as JT
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.CallBPPInternal as CallBPPInternal (CalculateFareReq (..), FareData (..), GetFareResponse (..), getFare)
import SharedLogic.Cancel
import qualified SharedLogic.CreateFareForMultiModal as CFFM
import SharedLogic.Search
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import TransactionLogs.Types

instance JT.JourneyLeg TaxiLegRequest m where
  search (TaxiLegRequestSearch TaxiLegRequestSearchData {..}) = do
    legSearchReq <- mkOneWaySearchReq
    dSearchRes <-
      DSearch.search
        journey.riderId
        legSearchReq
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        False
        True
        multimodalSearchRequestId
    upsertJourneyLegAction dSearchRes.searchRequest.id.getId
    QJourneyLeg.updateDistanceAndDuration (convertMetersToDistance Meter <$> dSearchRes.distance) dSearchRes.duration journeyLegData.id
    fork "search cabs" . withShortRetry $ do
      becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
      let generatedJson = encode becknTaxiReqV2
      logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
      void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 journey.merchantId
    return $ JT.SearchResponse {id = dSearchRes.searchRequest.id.getId}
    where
      lastAndRest :: [a] -> Maybe (a, [a])
      lastAndRest [] = Nothing -- Handle empty list case
      lastAndRest xs = Just (last xs, init xs)

      mkOneWaySearchReq = do
        (destination, stops') <- lastAndRest stops & fromMaybeM (InvalidRequest "Destination is required!")
        return $
          OneWaySearch $
            OneWaySearchReq
              { origin = origin,
                isSourceManuallyMoved = Just False,
                isDestinationManuallyMoved = Just False,
                isSpecialLocation = Just False, -- Fix it later
                startTime = journeyLegData.fromDepartureTime,
                isReallocationEnabled = Just True,
                fareParametersInRateCard = Just True,
                quotesUnifiedFlow = Just True,
                sessionToken = Nothing,
                placeNameSource = Nothing,
                driverIdentifier = Nothing,
                stops = Just stops',
                destination = Just destination,
                isMeterRideSearch = Just False,
                recentLocationId = Nothing,
                platformType = Nothing,
                isReserveRide = Just False,
                subscriptionId = Nothing,
                verifyBeforeCancellingOldBooking = Just True,
                numberOfLuggages = Nothing,
                doMultimodalSearch = Just True,
                ..
              }
  search _ = throwError (InternalError "Not Supported")

  confirm (TaxiLegRequestConfirm req) = do
    let shouldBookLater = req.bookLater || (not req.forcedBooked)
    unless shouldBookLater $ do
      now <- getCurrentTime
      QSearchRequest.updateStartTime (Id req.searchId) now
      mbEstimate <- maybe (pure Nothing) QEstimate.findById req.estimateId
      case mbEstimate of
        Just estimate -> do
          withTryCatch "cancelSearch:TaxiConfirm" (cancelSearchUtil (req.personId, req.merchantId) estimate.id)
            >>= \case
              Left err -> do
                logTagInfo "Failed to cancel" $ show err
                pure ()
              Right _ -> pure ()
          let selectReq =
                DSelect.DSelectReq
                  { customerExtraFee = Nothing,
                    isPetRide = Nothing,
                    customerExtraFeeWithCurrency = Nothing,
                    autoAssignEnabled = True,
                    autoAssignEnabledV2 = Just True,
                    paymentMethodId = Nothing,
                    paymentInstrument = Nothing,
                    otherSelectedEstimates = Nothing,
                    isAdvancedBookingEnabled = Nothing,
                    deliveryDetails = Nothing,
                    disabilityDisable = Nothing,
                    billingCategory = Nothing,
                    preferSafetyPlus = Nothing
                  }
          void $ DSelect.select2' (req.personId, req.merchantId) estimate.id selectReq
        Nothing -> CFFM.setConfirmOnceGetFare req.searchId
  confirm _ = throwError (InternalError "Not Supported")

  update (TaxiLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not Supported")

  cancel (TaxiLegRequestCancel legData) = do
    mbBooking <- QBooking.findByTransactionIdAndStatus legData.searchRequestId.getId activeBookingStatus
    case mbBooking of
      Just booking -> do
        maybe
          ( do
              mbRide <- QRide.findByRBId booking.id
              let cancelReq =
                    DCancel.CancelReq
                      { reasonCode = legData.reasonCode,
                        reasonStage = SCR.OnAssign,
                        additionalInfo = legData.additionalInfo,
                        reallocate = legData.reallocate,
                        blockOnCancellationRate = legData.blockOnCancellationRate
                      }
              dCancelRes <- DCancel.cancel booking mbRide cancelReq legData.cancellationSource
              void $ withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes cancelReq.reallocate
          )
          ( \estimateId -> do
              withTryCatch "cancelSearch:TaxiCancel" (cancelSearch' (booking.riderId, booking.merchantId) estimateId)
                >>= \case
                  Left err -> do
                    logTagInfo "Failed to cancel booking search: " $ show err
                    pure ()
                  Right _ -> pure ()
          )
          legData.cancelEstimateId
      Nothing -> do
        searchReq <- QSearchRequest.findById legData.searchRequestId >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legData.searchRequestId.getId)
        case legData.journeyLeg.legPricingId of
          Just pricingId -> do
            withTryCatch "cancelSearch:TaxiCancel" (cancelSearch' (searchReq.riderId, searchReq.merchantId) (Id pricingId))
              >>= \case
                Left err -> do
                  logTagInfo "Failed to cancel estimate search: " $ show err
                  pure ()
                Right _ -> pure ()
          Nothing -> return ()
  cancel _ = throwError (InternalError "Not Supported")

  getState (TaxiLegRequestGetState req) = do
    mbBooking <- QBooking.findByTransactionIdAndStatus req.searchId.getId (activeBookingStatus <> [COMPLETED])
    mbRide <- maybe (pure Nothing) (QRide.findByRBId . (.id)) mbBooking
    mbEstimate <-
      case mbBooking of
        Just _ -> return Nothing
        Nothing -> do
          maybe (pure Nothing) (QEstimate.findById . Id) req.journeyLeg.legPricingId

    vehiclePosition <- JT.getTaxiVehiclePosition mbRide
    (oldStatus, bookingStatus, trackingStatus, trackingStatusLastUpdatedAt) <- JMStateUtils.getTaxiAllStatuses req.journeyLeg mbBooking mbRide mbEstimate
    return $
      JT.Single $
        JT.JourneyLegStateData
          { status = oldStatus,
            bookingStatus = bookingStatus,
            trackingStatus,
            trackingStatusLastUpdatedAt,
            userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
            vehiclePositions = maybe [] (\latLong -> [JT.VehiclePosition {position = Just latLong, vehicleId = "taxi", upcomingStops = [], route_state = Nothing}]) vehiclePosition,
            legOrder = req.journeyLeg.sequenceNumber,
            subLegOrder = 1,
            mode = DTrip.Taxi,
            fleetNo = Nothing
          }
  getState _ = throwError (InternalError "Not Supported")

  getInfo (TaxiLegRequestGetInfo req) = do
    mbBooking <- QBooking.findByTransactionIdAndStatus req.searchId.getId (activeBookingStatus <> [COMPLETED])
    case mbBooking of
      Just booking -> do
        mRide <- QRide.findByRBId booking.id
        Just <$> JT.mkLegInfoFromBookingAndRide booking mRide req.journeyLeg
      Nothing -> do
        QSearchRequest.findById req.searchId
          >>= \case
            Just searchReq -> Just <$> JT.mkLegInfoFromSearchRequest searchReq req.journeyLeg
            Nothing -> return Nothing
  getInfo _ = throwError (InternalError "Not Supported")

  getFare (TaxiLegRequestGetFare taxiGetFareData) = do
    let calculateFareReq =
          CallBPPInternal.CalculateFareReq
            { pickupLatLong = LatLong {lat = taxiGetFareData.startLocation.latitude, lon = taxiGetFareData.startLocation.longitude},
              dropLatLong = Just $ LatLong {lat = taxiGetFareData.endLocation.latitude, lon = taxiGetFareData.endLocation.longitude},
              mbDistance = Just $ distanceToMeters taxiGetFareData.distance,
              mbDuration = Just taxiGetFareData.duration,
              mbTripCategory = Nothing
            }
    fareData <- CallBPPInternal.getFare taxiGetFareData.merchant taxiGetFareData.merchantOpCity.city calculateFareReq
    let mbFare = listToMaybe $ sortBy (comparing CallBPPInternal.minFare <> comparing CallBPPInternal.maxFare) (CallBPPInternal.estimatedFares fareData)
    return (True, mbFare <&> \taxi -> JT.GetFareResponse {estimatedMinFare = taxi.minFare, estimatedMaxFare = taxi.maxFare, liveVehicleAvailableServiceTypes = Nothing, possibleRoutes = Nothing})
  getFare _ = throwError (InternalError "Not Supported")

-- moved these here to avoid cyclic dependencies
cancelSearch' ::
  ( DSearch.SearchRequestFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  (Id DPerson.Person, Id Merchant.Merchant) ->
  Id DEstimate.Estimate ->
  m CancelAPIResponse
cancelSearch' (personId, merchantId) estimateId = withPersonIdLogTag personId $ cancelSearchUtil (personId, merchantId) estimateId
