{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Taxi where

-- import qualified API.UI.Select as DSelect
-- import qualified Domain.Action.UI.Search as DSearch
import Kernel.Prelude
-- import qualified Domain.Types.JourneyLeg as DJourenyLeg
-- import qualified Domain.Types.SearchRequest as DSR

-- import qualified SharedLogic.CallBPPInternal as CallBPPInternal
-- import qualified Storage.Queries.Booking as QBooking
-- import qualified Storage.Queries.Estimate as QEstimate
-- import qualified Storage.Queries.Journey as QJourney
-- import qualified Storage.Queries.Ride as QRide
-- import qualified Storage.Queries.SearchRequest as QSearchRequest
-- import Kernel.External.Maps.Types
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Taxi
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg TaxiLegRequest m where
  search (TaxiLegRequestSearch _) = return ()
  search _ = throwError (InternalError "Not Supported")

  -- let journeySearchData = mkJourneySearchData
  -- legSearchReq <- mkOneWaySearchReq
  -- dSearchRes <-
  --   DSearch.search
  --     parentSearchReq.riderId
  --     legSearchReq
  --     parentSearchReq.bundleVersion
  --     parentSearchReq.clientVersion
  --     parentSearchReq.clientConfigVersion
  --     parentSearchReq.clientRnVersion
  --     parentSearchReq.clientId
  --     parentSearchReq.device
  --     False
  --     (Just journeySearchData)
  -- fork "search cabs" . withShortRetry $ do
  --   becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
  --   let generatedJson = encode becknTaxiReqV2
  --   logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
  --   void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 parentSearchReq.merchantId
  -- where
  --   lastAndRest :: [a] -> Maybe (a, [a])
  --   lastAndRest [] = Nothing -- Handle empty list case
  --   lastAndRest xs = Just (last xs, init xs)

  --   mkOneWaySearchReq = do
  --     (destination, stops) <- lastAndRest stops & fromMaybeM (InvalidRequest "Destination is required!")
  --     return $
  --       OneWaySearch $
  --         OneWaySearchReq
  --           { origin = origin,
  --             isSourceManuallyMoved = False,
  --             isDestinationManuallyMoved = False,
  --             isSpecialLocation = False, -- Fix it later
  --             startTime = journeyLegData.fromDepartureTime,
  --             isReallocationEnabled = parentSearchReq.isReallocationEnabled,
  --             quotesUnifiedFlow = parentSearchReq.quotesUnifiedFlow,
  --             sessionToken = parentSearchReq.sessionToken,
  --             placeNameSource = parentSearchReq.placeNameSource,
  --             driverIdentifier = Nothing,
  --             ..
  --           }

  --   mkJourneySearchData =
  --     JourneySearchData
  --       { journeyId = journeyLegData.journeyId,
  --         journeyLegOrder = journeyLegData.sequenceNumber,
  --         agency = journeyLegData.agency,
  --         skipBooking = False,
  --         convenienceCost = Nothing,
  --         pricingId = Nothing
  --       }

  confirm (TaxiLegRequestConfirm _) = return ()
  confirm _ = throwError (InternalError "Not Supported")

  -- now <- getCurrentTime
  -- let shouldSkipBooking = req.skipBooking || (floor (diffUTCTime newTime oldTime) :: Integer) >= 300 -- 5 minutes buffer
  -- unless shouldSkipBooking $ do
  --   let selectReq =
  --         DSelect.DSelectReq
  --           { customerExtraFee = Nothing,
  --             customerExtraFeeWithCurrency = Nothing,
  --             autoAssignEnabled = True,
  --             autoAssignEnabledV2 = Just True,
  --             paymentMethodId = Nothing,
  --             otherSelectedEstimates = Nothing,
  --             isAdvancedBookingEnabled = Nothing,
  --             deliveryDetails = Nothing,
  --             disabilityDisable = Nothing
  --           }
  --   void $ DSelect.select2' (req.personId, req.merchantId) req.estimateId selectReq

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

  cancel (TaxiLegRequestCancel _legData) = return ()
  cancel _ = throwError (InternalError "Not Supported")

  getState (TaxiLegRequestGetState _req) = return $ JT.JourneyLegState {status = JT.InPlan, currentPosition = Nothing}
  getState _ = throwError (InternalError "Not Supported")

  -- mbBooking <- QBooking.findByTransactionId req.searchId.getId
  -- case mbBooking of
  --   Just booking -> do
  --     mbRide <- QRide.findByRBId booking.Id
  --     let journeyLegStatus = JT.getTexiLegStatusFromBooking booking mbRide
  --     return $ JT.JourneyLegState {status = journeyLegStatus, currentPosition = Nothing}
  --   Nothing -> do
  --     searchReq <- QSearchRequest.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
  --     journeyLegInfo <- searchReq.journeySearchData & fromMaybeM (InvalidRequest "JourneySearchData not found")
  --     let journeyLegStatus = JT.getTexiLegStatusFromSearch journeyLegInfo
  --     return $ JT.JourneyLegState {status = journeyLegStatus, currentPosition = Nothing}

  getInfo (TaxiLegRequestGetInfo _req) = throwError (InternalError "Not Supported")
  getInfo _ = throwError (InternalError "Not Supported")

  -- mbBooking <- QBooking.findByTransactionId req.searchId.getId
  -- case mbBooking of
  --   Just booking -> do
  --     mRide <- QRide.findByRBId booking.Id
  --     JT.mkLegInfoFromBookingAndRide booking mRide
  --   Nothing -> do
  --     searchReq <- QSearchRequest.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
  --     JT.mkLegInfoFromSearchRequest searchReq

  getFare (TaxiLegRequestGetFare _taxiGetFareData) = throwError (InternalError "Not Supported")
  getFare _ = throwError (InternalError "Not Supported")

-- let calculateFareReq =
--       CallBPPInternal.CalculateFareReq
--         { pickupLatLong = LatLong {lat = taxiGetFareData.startLocation.latLng.latitude, lon = taxiGetFareData.startLocation.latLng.longitude},
--           dropLatLong = LatLong {lat = taxiGetFareData.endLocation.latLng.latitude, lon = taxiGetFareData.endLocation.latLng.longitude},
--           mbDistance = Just $ distanceToMeters taxiGetFareData.distance,
--           mbDuration = Just taxiGetFareData.duration
--         }
-- fareData <- CallBPPInternal.getFare taxiGetFareData.merchant taxiGetFareData.merchantOpCity.city calculateFareReq
-- return JT.GetFareResponse {estimatedMinFare = fareData.minFare, estimatedMaxFare = fareData.maxFare}
