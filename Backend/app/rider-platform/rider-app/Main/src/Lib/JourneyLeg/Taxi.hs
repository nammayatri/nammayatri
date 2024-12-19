module Lib.JourneyLeg.Taxi where

mapRideStatusToJourneyLegStatus :: RideStatus -> JourneyLegStatus
mapRideStatusToJourneyLegStatus status = case status of
    UPCOMING    -> InPlan
    NEW         -> Booked
    INPROGRESS  -> Ongoing
    COMPLETED   -> Completed
    CANCELLED   -> Cancelled

data TaxiSearchData = TaxiSearchData
  { personId: Id Person
  , bundleVersion :: Maybe Text
  , fromLocation :: Location
  , stops :: [Location]
  }

data TaxiLegUpdateVariantData  = TaxiLegUpdateVariantData
  { searchRequestId :: Id SearchRequest
  , estimateId :: Id Estimate
  , merchantId :: Id Merchant
  , personId :: Id Person
  }

data TaxiLegUpdateData = EditLocation Location EditLocationReq | TaxiUpdateStartTime UTCTime | UpdateVariant TaxiLegUpdateVariantData

data TaxiConfirmData = TaxiConfirmData
  { skipBooking :: Bool,
    journeyLegOrder :: Int
  }

data TaxiLegRequest = TaxiLegRequestSearch MultiModalLeg TaxiSearchData | TaxiLegRequestConfirm TaxiConfirmData | TaxiLegRequestUpdate TaxiLegUpdateData (Id LegID) 

instance JourneyLeg TaxiLeg m where
  search (TaxilegRequestSearch multimodalLeg taxiLegSearchData) = do
    QSR.create multimodalLeg taxiLegSearchData
    dSearchRes <- search personId legSearchReq bundleVersion clientVersion clientConfigVersion_ clientRnVersion clientId device isDashboardRequest_ (Just journeySearchData)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  confirm (TaxilegRequestConfirm taxiLegConfirmData) = do
    searchReq <- QSearchRequest.findById taxiLegConfirmData.searchReqId
    estimateId <- searchReq.journeySearchData.pricingId
    case taxiLegConfirmData.skippedBooking of
      True ->
        -- subtract price from total estimate
        QSearchRequest.updateIsSkipped True
      False ->
        void $ Select.select pId (Id estimateId) selectReq
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
    -- call cancelV2
    -- update JourneyLegStatus: Cancelled
  getState (TaxiLeg _legData) = return InPlan
  get (TaxiLeg _legData) = return _legData
