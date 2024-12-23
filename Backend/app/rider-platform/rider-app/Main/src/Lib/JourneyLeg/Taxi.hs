module Lib.JourneyLeg.Taxi where

import qualified API.UI.Select as DSelect
import qualified Domain.Types.JourneyLeg as DJourenyLeg
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.SearchRequest as DSR
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Lib.JourneyLeg.Types as JT

mapRideStatusToJourneyLegStatus :: RideStatus -> JourneyLegStatus
mapRideStatusToJourneyLegStatus status = case status of
  UPCOMING    -> InPlan
  NEW         -> Booked
  INPROGRESS  -> Ongoing
  COMPLETED   -> Completed
  CANCELLED   -> Cancelled

data TaxiLegUpdateVariantData  = TaxiLegUpdateVariantData
  { searchRequestId :: Id SearchRequest
  , estimateId :: Id Estimate
  , merchantId :: Id Merchant
  , personId :: Id Person
  }

data TaxiLegUpdateData = EditLocation Location EditLocationReq | TaxiUpdateStartTime UTCTime | UpdateVariant TaxiLegUpdateVariantData

data TaxiSearchRequestData = TaxiSearchRequestData
  { origin :: SearchReqLocation
  , stops :: [SearchReqLocation]
  }

data TaxiLegConfirmRequest = TaxiLegConfirmReques
  { skipBooking :: Bool,
    personId :: Id Person,
    merchantId :: Id Merchant,
    estimateId :: Id Estimate
  }

data TaxiLegRequest 
  = TaxiLegRequestSearch DSR.SearchRequest DJourenyLeg.JourneyLeg TaxiSearchRequestData
  | TaxiLegConfirm TaxiLegConfirmRequest
  | TaxiLegRequestUpdate TaxiLegUpdateData (Id LegID)
  | TaxiLegGetState (Id DSR.SearchRequest)

instance JourneyLeg TaxiLegRequest m where
  search (TaxilegRequestSearch parentSearchReq multimodalLeg taxiLegSearchData) = do
    let journeySearchData = mkJourneySearchData
    legSearchReq <- mkOneWaySearchReq
    dSearchRes <- 
      DSearch.search
        parentSearchReq.riderId
        legSearchReq
        parentSearchReq.bundleVersion
        parentSearchReq.clientVersion
        parentSearchReq.clientConfigVersion
        parentSearchReq.clientRnVersion
        parentSearchReq.clientId
        parentSearchReq.device
        False
        (Just journeySearchData)
    fork "search cabs" . withShortRetry $ do
      becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
      let generatedJson = encode becknTaxiReqV2
      logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
      void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 taxiLegSearchData.merchantId
    where
      lastAndRest :: [a] -> Maybe (a, [a])
      lastAndRest [] = Nothing  -- Handle empty list case
      lastAndRest xs = Just (last xs, init xs)
     
      mkOneWaySearchReq = do
        (destination, stops) <- lastAndRest taxiLegSearchData.stops & fromMaybeM (InvalidRequest "Destination is required!")
        return $ 
          OneWaySearch $
            OneWaySearchReq
              { origin = taxiLegSearchData.origin
                isSourceManuallyMoved = False,
                isDestinationManuallyMoved = False,
                isSpecialLocation = False, -- Fix it later
                startTime = multimodalLeg.fromDepartureTime,
                isReallocationEnabled = parentSearchReq.isReallocationEnabled,
                quotesUnifiedFlow = parentSearchReq.quotesUnifiedFlow,
                sessionToken = parentSearchReq.sessionToken,
                placeNameSource = parentSearchReq.placeNameSource,
                driverIdentifier = Nothing,
                ..
              }

      mkJourneySearchData = 
        JourneySearchData
          { journeyId = multimodalLeg.journeyId,
            journeyLegOrder :: Int,
            agency = multimodalLeg.agency,
            skipBooking = False,
            convenienceCost = Nothing,
            pricingId = Nothing
          }
  
  confirm (TaxiLegConfirm req) = do
    now <- getCurrentTime
    let shouldSkipBooking = req.skipBooking || (floor (diffUTCTime newTime oldTime) :: Integer) >= 300 -- 5 minutes buffer
    unless shouldSkipBooking $
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
      void $ DSelect.select2' (req.personId, req.merchantId) req.estimateId selectReq
  
  update (TaxiLegRequest $ TaxiLegRequestUpdate taxiLegUpdateRequest legId) =
    -- Handle the specific type of update
    case taxiLegUpdateRequest of
        EditLocation EditLocationReq -> do
            -- Handle edit pickup and edit destination flow
            editLocation rideId  (personId, merchantId) editLocationReq
            return ()
        TaxiUpdateStartTime newStartTime -> do
          -- Cancel previous scheduled ride and create new search and then confirm
          return ()
        UpdateVariant newVariant -> do
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
            return ()

  cancel (TaxiLeg _legData) = return ()
  
  getState (TaxiLegGetState searchId) = do
     searchReq <- QSearchRequest.findById searchId >>= fromMaybeM ("Internal Error in searchId :" <> searchId)
     booking <- QBooking.findByTransactionId searchId
     ride <- QRide.findByRBId booking.id
     case (booking, ride) of
       (Just bookings , Just rides) -> do
           let journeyLegStatus = mapRideStatusToJourneyLegStatus rides.status
           return $ JT.JourneyLegState {status = journeyLegStatus, currentPosition = Nothing}
        (Just bookings , Nothing) -> do
           return $ JT.JourneyLegState { status = Assigning , currentPosition = Nothing }
        (_, _) -> do
           isSkipped <- searchReq.journeyLegInfo.isSkipped
           isCancelled <- searchReq.journeyLegInfo.isCancelled
           case (isSkipped, isCancelled) of
             (False, False) -> return $ JT.JourneyLegState { status = InPlan , currentPosition = Nothing}
             (True, False) ->  return $ JT.JourneyLegState { status = Skipped , currentPosition = Nothing}
             (_, _) ->  return $ JT.JourneyLegState { status = Cancelled, currentPosition = Nothing }

  get (TaxiLegGetState srId) = do
    mbBooking <- QBooking.findByTransactionId srId
    case mbBooking of
      Just booking <- do
        mRide <- QRide.findByRBId booking.Id
        mkLegInfoFromBookingAndRide booking mbRide
      Nothing -> do
        searchReq <- QSearchRequest.findById srId
        mkLegInfoFromSearchRequest searchReq

