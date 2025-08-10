{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Taxi where

import qualified API.UI.Select as DSelect
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Cancel as CACL
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
import qualified Domain.Types.EstimateStatus as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RouteDetails as RD
import Domain.Types.ServiceTierType ()
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps.Types
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import Lib.JourneyLeg.Types
import Lib.JourneyLeg.Types.Taxi
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import qualified Lib.JourneyModule.Types as JT
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.CallBPPInternal as CallBPPInternal (CalculateFareReq (..), FareData (..), GetFareResponse (..), getFare)
import qualified SharedLogic.CreateFareForMultiModal as CFFM
import SharedLogic.Search
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import TransactionLogs.Types

instance JT.JourneyLeg TaxiLegRequest m where
  search (TaxiLegRequestSearch TaxiLegRequestSearchData {..}) = do
    let journeySearchData = mkJourneySearchData
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
        (Just journeySearchData)
        (Just journeyLegData.id)
        (listToMaybe journeyLegData.routeDetails)
        True
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
                ..
              }

      mkJourneySearchData =
        JourneySearchData
          { journeyId = journeyLegData.journeyId.getId,
            journeyLegOrder = journeyLegData.sequenceNumber,
            agency = journeyLegData.agency <&> (.name),
            skipBooking = False,
            convenienceCost = 0,
            pricingId = Nothing,
            isDeleted = Just False,
            onSearchFailed = Nothing
          }
  search _ = throwError (InternalError "Not Supported")

  confirm (TaxiLegRequestConfirm req) = do
    let shouldSkipBooking = req.skipBooking || (not req.forcedBooked)
    unless shouldSkipBooking $ do
      now <- getCurrentTime
      QSearchRequest.updateStartTime (Id req.searchId) now
      mbEstimate <- maybe (pure Nothing) QEstimate.findById req.estimateId
      case mbEstimate of
        Just estimate -> do
          when (estimate.status == DEstimate.NEW) $ do
            let selectReq =
                  DSelect.DSelectReq
                    { customerExtraFee = Nothing,
                      isPetRide = Nothing,
                      customerExtraFeeWithCurrency = Nothing,
                      autoAssignEnabled = True,
                      autoAssignEnabledV2 = Just True,
                      paymentMethodId = Nothing,
                      otherSelectedEstimates = Nothing,
                      isAdvancedBookingEnabled = Nothing,
                      deliveryDetails = Nothing,
                      disabilityDisable = Nothing,
                      preferSafetyPlus = Nothing
                    }
            void $ DSelect.select2' (req.personId, req.merchantId) estimate.id selectReq
        Nothing -> CFFM.setConfirmOnceGetFare req.searchId
  confirm _ = throwError (InternalError "Not Supported")

  update (TaxiLegRequestUpdate _taxiLegUpdateRequest) = return ()
  update _ = throwError (InternalError "Not Supported")

  -- case taxiLegUpdateRequest of
  --   EditLocation editLocationRequest -> do
  --     let editLocationReq =
  --           DRide.EditLocationReq
  --             { origin = editLocationRequest.origin,
  --               destination = editLocationRequest.destination
  --             }
  --     void $ editLocation editLocationRequest.rideId (editLocationRequest.personId, editLocationRequest.merchantId) editLocationReq
  --   UpdateVariant taxiLegUpdateVariant -> do
  --     searchRequest <- QSearchRequest.findById taxiLegUpdateVariant.searchRequestId >>= fromMaybeM (InvalidRequest "SearchRequest not found")
  --     journeyLegInfo <- searchRequest.x & fromMaybeM (InvalidRequest "Journey Leg for SearchRequest not found")
  --     oldEstimateId <- journeyLegInfo.pricingId & fromMaybeM (InternalError "Old estimate id not found for search request")
  --     oldEstimate <- QEstimate.findById (Id oldEstimateId) >>= fromMaybeM (InternalError "Old estimate not found for search request")
  --     newEstimate <- QEstimate.findById taxiLegUpdateVariant.estimateId >>= fromMaybeM (InvalidRequest "New Estimate requested not found")
  --     QSearchRequest.updatePricingId taxiLegUpdateVariant.searchRequestId (Just (taxiLegUpdateVariant.estimateId).getId)
  --     let journeyId = journeyLegInfo.journeyId
  --     journey <- QJourney.findByPrimaryKey (Id journeyId) >>= fromMaybeM (InvalidRequest "Journey not found")
  --     initialFare <- journey.estimatedFare & fromMaybeM (InvalidRequest "Journey for SearchRequest not found")
  --     price1 <- initialFare `subtractPrice` oldEstimate.estimatedTotalFare
  --     newEstimatedPrice <- price1 `addPrice` newEstimate.estimatedTotalFare
  --     QJourney.updateEstimatedFare (Just newEstimatedPrice) (Id journeyId)

  cancel (TaxiLegRequestCancel legData) = do
    mbBooking <- QBooking.findByTransactionId legData.searchRequestId.getId
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
          (\estimateId -> void $ cancelSearch' (booking.riderId, booking.merchantId) estimateId)
          legData.cancelEstimateId
      Nothing -> do
        searchReq <- QSearchRequest.findById legData.searchRequestId >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legData.searchRequestId.getId)
        journeySearchData <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest $ "JourneySearchData not found for search id: " <> searchReq.id.getId)
        case journeySearchData.pricingId of
          Just pricingId -> do
            void $ cancelSearch' (searchReq.riderId, searchReq.merchantId) (Id pricingId)
          Nothing -> return ()
    if legData.isSkipped then QJourneyLeg.updateIsSkipped (Just True) (Just legData.searchRequestId.getId) else QJourneyLeg.updateIsDeleted (Just True) (Just legData.searchRequestId.getId)
  cancel _ = throwError (InternalError "Not Supported")

  isCancellable ((TaxiLegRequestIsCancellable legData)) = do
    mbBooking <- QBooking.findByTransactionId legData.searchId.getId
    case mbBooking of
      Just booking -> do
        mbRide <- QRide.findByRBId booking.id
        canCancel <- DCancel.isBookingCancellable booking mbRide
        return $ JT.IsCancellableResponse {canCancel}
      Nothing -> do
        return $ JT.IsCancellableResponse {canCancel = True}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (TaxiLegRequestGetState req) = do
    mbBooking <- QBooking.findByTransactionIdAndStatus req.searchId.getId (activeBookingStatus <> [COMPLETED, CANCELLED])
    case mbBooking of
      Just booking -> do
        mbRide <- QRide.findByRBId booking.id
        (journeyLegStatus, vehiclePosition) <- JT.getTaxiLegStatusFromBooking booking mbRide
        journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
        journeyLegsStatus <- getJourneyLegStatus booking.journeyRouteDetails (Just booking.transactionId) Nothing

        return $
          JT.Single $
            JT.JourneyLegStateData
              { status = journeyLegStatus,
                legStatus = JT.TaxiStatusElement <$> journeyLegsStatus,
                userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
                vehiclePositions = maybe [] (\latLong -> [JT.VehiclePosition {position = Just latLong, vehicleId = "taxi", route_state = Nothing, upcomingStops = []}]) vehiclePosition,
                legOrder = journeyLegOrder,
                subLegOrder = 1,
                mode = DTrip.Taxi
              }
      Nothing -> do
        searchReq <- QSearchRequest.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
        journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "JourneySearchData not found")
        mbEstimate <- maybe (pure Nothing) (QEstimate.findById . Id) journeyLegInfo.pricingId
        journeyLegStatus <- JT.getTaxiLegStatusFromSearch searchReq
        journeyLegsStatus <- getJourneyLegStatus searchReq.journeyRouteDetails (Just searchReq.id.getId) (mbEstimate <&> (.id.getId))

        return $
          JT.Single $
            JT.JourneyLegStateData
              { status = journeyLegStatus,
                legStatus = JT.TaxiStatusElement <$> journeyLegsStatus,
                userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
                vehiclePositions = [],
                legOrder = journeyLegInfo.journeyLegOrder,
                subLegOrder = 1,
                mode = DTrip.Taxi
              }
    where
      getJourneyLegStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe RD.RouteDetails -> Maybe Text -> Maybe Text -> m (Maybe JT.JourneyTaxiLegStatus)
      getJourneyLegStatus journeyRouteDetails mbLegSearchId mbPricingId = do
        bookingStatus <- JMStateUtils.getTaxiJourneyBookingStatus mbLegSearchId mbPricingId
        mapM
          ( \routeDetails -> do
              trackingStatus <- JMStateUtils.getTaxiJourneyLegTrackingStatus mbLegSearchId mbPricingId routeDetails
              return $ JT.JourneyTaxiLegStatus {trackingStatus = trackingStatus, bookingStatus = bookingStatus}
          )
          journeyRouteDetails
  getState _ = throwError (InternalError "Not Supported")

  getInfo (TaxiLegRequestGetInfo req) = do
    mbBooking <- QBooking.findByTransactionIdAndStatus req.searchId.getId (activeBookingStatus <> [COMPLETED, CANCELLED])
    case mbBooking of
      Just booking -> do
        mRide <- QRide.findByRBId booking.id
        Just <$> JT.mkLegInfoFromBookingAndRide booking mRide req.journeyLeg.entrance req.journeyLeg.exit
      Nothing -> do
        mbSearchReq <- QSearchRequest.findById req.searchId
        if isNothing mbSearchReq && req.ignoreOldSearchRequest
          then return Nothing
          else do
            searchReq <- fromMaybeM (SearchRequestNotFound req.searchId.getId) mbSearchReq
            Just <$> JT.mkLegInfoFromSearchRequest searchReq req.journeyLeg.entrance req.journeyLeg.exit
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
    return (True, mbFare <&> \taxi -> JT.GetFareResponse {estimatedMinFare = taxi.minFare, estimatedMaxFare = taxi.maxFare, serviceTypes = Nothing})
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
  m DSelect.CancelAPIResponse
cancelSearch' (personId, merchantId) estimateId = withPersonIdLogTag personId $ cancelSearchUtil (personId, merchantId) estimateId

cancelSearchUtil ::
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
  m DSelect.CancelAPIResponse
cancelSearchUtil (personId, merchantId) estimateId = do
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  activeBooking <- B.runInReplica $ QRB.findByTransactionIdAndStatus estimate.requestId.getId activeBookingStatus
  if isJust activeBooking
    then do
      logTagInfo "Booking already created while cancelling estimate." estimateId.getId
      throwError (ActiveBookingPresent estimateId.getId)
    else do
      dCancelSearch <- DCancel.mkDomainCancelSearch personId estimateId
      result <-
        try @_ @SomeException $
          when dCancelSearch.sendToBpp . void . withShortRetry $ do
            CallBPP.cancelV2 merchantId dCancelSearch.providerUrl =<< CACL.buildCancelSearchReqV2 dCancelSearch
      case result of
        Left err -> do
          logTagInfo "Failed to cancel" $ show err
          throwError (FailedToCancelSearch estimateId.getId)
        Right _ -> do
          DCancel.cancelSearch personId dCancelSearch
          pure DSelect.Success
