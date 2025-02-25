{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Taxi where

import qualified API.UI.Select as DSelect
import qualified API.UI.Select as Select
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
import qualified Data.Text as T
import Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.Booking
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.ServiceTierType
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Types
import Lib.JourneyLeg.Types.Taxi
import qualified Lib.JourneyModule.Types as JT
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.CreateFareForMultiModal as CFFM
import SharedLogic.Search
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest

instance JT.JourneyLeg TaxiLegRequest m where
  search (TaxiLegRequestSearch TaxiLegRequestSearchData {..}) = do
    let journeySearchData = mkJourneySearchData
    legSearchReq <- mkOneWaySearchReq
    dSearchRes <-
      DSearch.search
        parentSearchReq.riderId
        legSearchReq
        parentSearchReq.clientBundleVersion
        parentSearchReq.clientSdkVersion
        parentSearchReq.clientConfigVersion
        parentSearchReq.clientReactNativeVersion
        parentSearchReq.clientId
        parentSearchReq.device
        False
        (Just journeySearchData)
    QJourneyLeg.updateDistanceAndDuration (convertMetersToDistance Meter <$> dSearchRes.distance) dSearchRes.duration journeyLegData.id
    fork "search cabs" . withShortRetry $ do
      becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
      let generatedJson = encode becknTaxiReqV2
      logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
      void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 parentSearchReq.merchantId
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
                placeNameSource = parentSearchReq.placeNameSource,
                driverIdentifier = Nothing,
                stops = Just stops',
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
            isDeleted = Just False
          }
  search _ = throwError (InternalError "Not Supported")

  confirm (TaxiLegRequestConfirm req) = do
    now <- getCurrentTime
    let shouldSkipBooking = req.skipBooking || (floor (diffUTCTime req.startTime now) :: Integer) >= 300 || (not req.forcedBooked) -- 5 minutes buffer
    unless shouldSkipBooking $ do
      mbEstimate <- maybe (pure Nothing) QEstimate.findById req.estimateId
      case mbEstimate of
        Just estimate -> do
          when (estimate.status == DEstimate.NEW) $ do
            let selectReq =
                  DSelect.DSelectReq
                    { customerExtraFee = Nothing,
                      customerExtraFeeWithCurrency = Nothing,
                      autoAssignEnabled = True,
                      autoAssignEnabledV2 = Just True,
                      paymentMethodId = Nothing,
                      otherSelectedEstimates = Nothing,
                      isAdvancedBookingEnabled = Nothing,
                      deliveryDetails = Nothing,
                      disabilityDisable = Nothing
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
        if legData.isSkipped then QBooking.updateisSkipped booking.id (Just True) else QBooking.updateIsCancelled booking.id (Just True)
      Nothing -> do
        searchReq <- QSearchRequest.findById legData.searchRequestId >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legData.searchRequestId.getId)
        journeySearchData <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest $ "JourneySearchData not found for search id: " <> searchReq.id.getId)
        case journeySearchData.pricingId of
          Just pricingId -> do
            cancelResponse <- Select.cancelSearch' (searchReq.riderId, searchReq.merchantId) (Id pricingId)
            case cancelResponse of
              DSelect.Success -> return ()
              DSelect.BookingAlreadyCreated -> throwError (InternalError $ "Cannot cancel search as booking is already created for searchId: " <> show searchReq.id.getId)
              DSelect.FailedToCancel -> throwError (InvalidRequest $ "Failed to cancel search for searchId: " <> show searchReq.id.getId)
          Nothing -> return ()
        if legData.isSkipped then QSearchRequest.updateSkipBooking legData.searchRequestId (Just True) else QSearchRequest.updateIsCancelled legData.searchRequestId (Just True)
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
    mbBooking <- QBooking.findByTransactionIdAndStatus req.searchId.getId (activeBookingStatus <> [COMPLETED])
    case mbBooking of
      Just booking -> do
        mbRide <- QRide.findByRBId booking.id
        (journeyLegStatus, vehiclePosition) <- JT.getTaxiLegStatusFromBooking booking mbRide
        journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
        return $
          JT.JourneyLegState
            { status = journeyLegStatus,
              userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
              vehiclePosition,
              nextStop = Nothing,
              nextStopTravelDistance = Nothing,
              nextStopTravelTime = Nothing,
              legOrder = journeyLegOrder,
              statusChanged = False,
              mode = DTrip.Taxi
            }
      Nothing -> do
        searchReq <- QSearchRequest.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
        journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "JourneySearchData not found")
        mbEstimate <- maybe (pure Nothing) (QEstimate.findById . Id) journeyLegInfo.pricingId
        let journeyLegStatus = JT.getTaxiLegStatusFromSearch journeyLegInfo (mbEstimate <&> (.status))
        return $
          JT.JourneyLegState
            { status = journeyLegStatus,
              userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
              vehiclePosition = Nothing,
              nextStop = Nothing,
              nextStopTravelDistance = Nothing,
              nextStopTravelTime = Nothing,
              legOrder = journeyLegInfo.journeyLegOrder,
              statusChanged = False,
              mode = DTrip.Taxi
            }
  getState _ = throwError (InternalError "Not Supported")

  getInfo (TaxiLegRequestGetInfo req) = do
    mbBooking <- QBooking.findByTransactionIdAndStatus req.searchId.getId (activeBookingStatus <> [COMPLETED])
    case mbBooking of
      Just booking -> do
        mRide <- QRide.findByRBId booking.id
        JT.mkLegInfoFromBookingAndRide booking mRide
      Nothing -> do
        searchReq <- QSearchRequest.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
        JT.mkLegInfoFromSearchRequest searchReq
  getInfo _ = throwError (InternalError "Not Supported")

  getFare (TaxiLegRequestGetFare taxiGetFareData) = do
    let calculateFareReq =
          CallBPPInternal.CalculateFareReq
            { pickupLatLong = LatLong {lat = taxiGetFareData.startLocation.latitude, lon = taxiGetFareData.startLocation.longitude},
              dropLatLong = LatLong {lat = taxiGetFareData.endLocation.latitude, lon = taxiGetFareData.endLocation.longitude},
              mbDistance = Just $ distanceToMeters taxiGetFareData.distance,
              mbDuration = Just taxiGetFareData.duration
            }
    fareData <- CallBPPInternal.getFare taxiGetFareData.merchant taxiGetFareData.merchantOpCity.city calculateFareReq
    let mbAutoFare = find (\f -> f.vehicleServiceTier == AUTO_RICKSHAW) fareData.estimatedFares
    return $ mbAutoFare <&> \auto -> JT.GetFareResponse {estimatedMinFare = auto.minFare, estimatedMaxFare = auto.maxFare}
  getFare _ = throwError (InternalError "Not Supported")
