module Domain.Types.JourneyLegs where

completedStatus :: [JourneyLegStatus]
completedStatus = [Cancelled, Completed]

data ServiceStatus = Pending | Failed | Confirmed | Verified | Cancelled deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RideStatus = UPCOMING | NEW | INPROGRESS | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

-- Mapper functions
data WalkLeg = WalkLeg MultiModalLeg

data TaxiLeg = TaxiLeg MultiModalLeg

data BusLeg = BusLeg MultiModalLeg

data MetroLeg = MetroLeg MultiModalLeg

data TrainLeg = TrainLeg MultiModalLeg

data TaxiSearchData = TaxiSearchData
  { personId: Id Person
  , bundleVersion :: Maybe Text
  , fromLocation :: Location
  , stops :: [Location]
  ... other things
  }

data TaxiLegUpdateVariantData  = TaxiLegUpdateVariantData
  {
    searchRequestId :: Id SearchRequest
  , estimateId :: Id Estimate
  , merchantId :: Id Merchant
  , personId :: Id Person
  }

data TaxiLegUpdateData = EditLocation Location EditLocationReq | TaxiUpdateStartTime UTCTime | UpdateVariant TaxiLegUpdateVariantData

data WalkSearchData = WalkSearchData
  { originAddress :: LocationAddress,
    destinationAddress :: LocationAddress
  , merchantId :: Id Merchant
  ... other things
  }

data TaxiSearchData =  TaxiSearchData
  { originAddress :: LocationAddress,
    destinationAddress :: LocationAddress,
    personId :: Id Person,
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    sessionToken :: Maybe Text
  }


data MetroSearchData = MetroSearchData   -- same for bus
  { personId :: Id Person,
    merchantId :: Id Person,
    originCity :: Context.City,
    quantity :: Int
  }

data TaxiConfirmData = TaxiConfirmData     -- same for other confirm reqs
  { skipBooking :: Bool,
    journeyLegOrder :: Int
  }

data WalkLegCancelRequest = WalkLegCancelRequest{searchId :: Id Walk}

data TaxiLegCancelRequest = TaxiLegCancelRequest{searchId :: Id Search}

data MetroLegCancelRequest = MetroLegCancelRequest{searchId :: Id FRFS}

data TaxiLegDeleteRequest = TaxiLegDeleteRequest{searchId :: Id Search}

data WalkLegDeleteRequest = WalkLegDeleteRequest{searchId :: Id Walk}

data MetroLegDeleteRequest = MetroLegDeleteRequest{searchId :: Id FRFS}

data WalkLegRequest = WalkLegRequestSearch MultiModalLeg WalkSearchData | WalkLegRequestUpdate WalkLegUpdateData | WalkLegRequestDelete WalkLegDeleteRequest
data TaxiLegRequest = TaxiLegRequestSearch MultiModalLeg TaxiSearchData | TaxiLegRequestConfirm TaxiConfirmData | TaxiLegRequestUpdate TaxiLegUpdateData (Id LegID) | TaxiLegRequestCancel TaxiLegCancelRequest | TaxiLegRequestDelete TaxiLegDeleteRequest
data MetroLegRequest = MetroLegRequestSearch MultiModalLeg MetroSearchData | MetroLegRequestConfirm MetroConfirmData | MetroLegRequestCancel MetroLegCancelRequest | MetroLegRequestDelete MetroLegDeleteRequest
data BusLegRequest = BusLegRequestSearch MultiModalLeg BusSearchData | BusLegRequestConfirm BusConfirmData

data WalkLegUpdateData = WalkLegUpdateData
  { id : Id LegID
    mbFromLocation : Location
    mbtoLocation : Location
    mbStartTime : Maybe UTCTime
    status : JourneyLegStatus
  }

data BusLegUpdateData = BusLegUpdateData
{
  id : Id LegID
  userLocation : Location
  busLocation : Location
  startTime : Maybe UTCTime
}
data BusLegRequest = BusLegRequestUpdate BusLegUpdateData

type SearchJourneyLeg leg m = leg -> m ()
type ConfirmJourneyLeg leg m = leg -> m ()
type CancelJourneyLeg leg m = leg -> m ()
type UpdateJourneyLeg leg m = leg -> m ()
type GetJourneyLegState leg m = leg -> m JourneyLegState
type GetJourneyLeg leg m = leg -> m MultiModalLeg

data Journey = Journey
  { ...
  , legs :: [JourneyLeg]
  }

data JourneyLeg = JourneyLeg
  { distance :: Distance.Distance,
    journeyId :: Id Journey,
    duration :: Time.Seconds,
    polyline :: GT.Polyline,
    mode :: GeneralVehicleType,
    startLocation :: GT.LocationV2,
    endLocation :: GT.LocationV2,
    fromStopDetails :: Maybe MultiModalStopDetails,
    toStopDetails :: Maybe MultiModalStopDetails,
    routeDetails :: Maybe MultiModalRouteDetails,
    agency :: Maybe MultiModalAgency,
    fromArrivalTime :: Maybe UTCTime,
    fromDepartureTime :: Maybe UTCTime,
    toArrivalTime :: Maybe UTCTime,
    toDepartureTime :: Maybe UTCTime
    status : JourneyLegStatus
    sequenceNumber : Int
  }

data GeneralVehicleType
  = Bus
  | MetroRail
  | Walk
  | Unspecified
  deriving (Show, Eq)

mkJourneyLeg :: JourneyLeg a m => MultiModalLeg -> m a
mkJourneyLeg multimodalLeg = do
  case multimodalLeg.mode of
    Bus -> BusLeg multimodalLeg
    MetroRail -> MetroLeg multimodalLeg
    Walk -> WalkLeg multimodalLeg
    Unspecified -> TaxiLeg multimodalLeg

class JourneyLeg leg m where
  search :: SearchJourneyLeg leg m
  confirm :: ConfirmJourneyLeg leg m
  update :: UpdateJourneyLeg leg m
  cancel :: CancelJourneyLeg leg m
  getState :: GetJourneyLegState leg m
  get :: GetJourneyLeg leg m


updateWalkLegById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Walkleg.Walkleg  ->  -> Id LegID -> m ()
updateWalkLegById (Domain.Types.Walkleg.Walkleg {..}) legId = do
   -- Function implementation goes here
    -- You can now use legId along with the fields of the Walkleg record

instance JourneyLeg WalkLeg m where
  search (WalkLegRequestSearch multimodalLeg walkLegSearchData) = do
    WL.create $ mkWalkLegSearch multimodalLeg walkLegSearchData
  confirm (WalkLeg _legData) = return () -- return nothing
  update (WalkLegRequest $ WalkLegRequestUpdate walkLegUpdateData) = do
    WL.updateWalkLegById $ walkLegUpdateData walkLegUpdateData.id
    -- WalkLegUpdateRequest :: id, mbFromLocation, mbtoLocation, mbStartTime
  cancel (WalkLegRequest $ WalkLegRequestCancel WalkLegCancelRequest) JourneyLegStatus =
    updateSearchRequestJourneyLeg(searchId, isCancelled, true)
    update JourneyLegStatus: Cancelled
  delete (WalkLegRequest $ WalkLegRequestDelete WalkLegDeleteRequest) JourneyLegStatus =
    cancel (WalkLegRequest $ WalkLegRequestCancel {searchId : searchId}) JourneyLegStatus
    updateSearchRequestJourneyLeg (searchId, isDelete, true)
    update JourneyLegStatus: Deleted
  getState (WalkLeg _legData) = return JourneyLegState -- id
  get (WalkLeg _legData) = return WalkLeg -- id

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

  cancel (TaxiLegRequest $ TaxiLegRequestCancel TaxiLegCancelRequest) JourneyLegStatus = do
    booking <- getBooking(searchId)
    case getStatus(searchId) of
      Booked | Delayed | Arriving -> do
        call cancel booking.id (booking.riderId, booking.merchantId)
        update JourneyLegStatus: Cancelled
        updateSearchRequestJourneyLeg (searchId, isCancel, true)
      InPlan -> update JourneyLegStatus: Skipped
      Assigning -> do
        call cancelSearch (booking.riderId, booking.merchantId)
        update JourneyLegStatus: Skipped
    updateSearchRequestJourneyLeg (searchId, isSkipped, true)
  delete (TaxiLegRequest $ TaxiLegRequestDelete TaxiLegDeleteRequest) JourneyLegStatus =
    cancel(TaxiLegRequest $ TaxiLegRequestCancel {searchId : searchId} JourneyLegStatus)
    updateSearchRequestJourneyLeg (searchId, isDelete, true)
    update JourneyLegStatus: Deleted
  getState (TaxiLeg _legData) = return InPlan
  get (TaxiLeg _legData) = return _legData

instance JourneyLeg BusLeg m where
  search (BusLegRequestSearch multimodalLeg busLegSearchData) = do
    FRFSSearch.create multimodalLeg busLegSearchData
    void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.BUS frfsSearchReq
  confirm (BusLegRequestConfirm busLegConfirmData) = do
    frfsSearchReq <- QFRFSSearch.findById taxiLegConfirmData.searchReqId
    quoteId <- searchReq.journeySearchData.pricingId
    case busLegConfirmData.skippedBooking of
      True ->
         -- subtract price from total estimate
        QFRFSSearch.updateIsSkipped True
      False ->
        void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)
  update (BusLegRequest $ BusLegRequestUpdate busLegUpdateRequest) =
  -- let customerLocation = get eta between user location and bus station
  -- let busLocation = get eta between bus and bus station
  -- let threshold = 50
      -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
      -- mark status with respect to user -  AtRiskOfMissing, Missed
    return ()
  cancel (BusLeg _legData) = return ()
  getState (BusLeg _legData) = return InPlan
  get (BusLeg _legData) = return _legData

instance JourneyLeg TrainLeg m where
  search (TrainLeg _legData) = return ()
  confirm (TrainLeg _legData) = return ()
  update (TrainLeg _legData) = return ()
  cancel (TrainLeg _legData) = return ()
  getState (TrainLeg _legData) = return InPlan
  get (TrainLeg _legData) = return _legData

instance JourneyLeg MetroLeg m where
  search (MetroLegRequestSearch multimodalLeg metroLegSearchData) = do
    FRFSSearch.create multimodalLeg metroLegSearchData
    void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.METRO frfsSearchReq
  confirm (MetroLegRequestConfirm metroLegConfirmData) = do
    frfsSearchReq <- QFRFSSearch.findById taxiLegConfirmData.searchReqId
    quoteId <- searchReq.journeySearchData.pricingId
    case metroLegConfirmData.skippedBooking of
      True ->
         -- subtract price from total estimate
        QFRFSSearch.updateIsSkipped True
      False ->
        void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)
  update (MetroLeg _legData) = return ()
  cancel (MetroLegRequest $ MetroLegRequestCancel MetroLegCancelRequest) JourneyLegStatus =
    booking <- getBooking(searchId)
    case getStatus(searchId) of
      Booked -> do
        call postFrfsBookingCanCancel (booking.riderId, booking.merchantId) booking.id
        update JourneyLegStatus: Cancelled
        updateSearchRequestJourneyLeg (searchId, isCancel, true)
      InPlan -> update JourneyLegStatus: Skipped
    updateSearchRequestJourneyLeg (searchId, isSkipped, true)
  delete(MetroLegRequest $ MetroLegRequestDelete MetroLegDeleteRequest) JourneyLegStatus =
    cancel(MetroLegRequest $ MetroLegRequestCancel {searchId : searchId}) JourneyLegStatus
    updateSearchRequestJourneyLeg (searchId, isDeleted, true)
    update JourneyLegStatus: Deleted
  getState (MetroLeg _legData) = return InPlan
  get (MetroLeg _legData) = return _legData

-- Journey Module (base function)
data JourneyInitData leg = JourneyInitData
  { legs: [leg]
  , parentSearchId :: Id SearchRequest
  , merchantId :: Id Merchant
  , merchantOperatingCityId :: Id MerchantOperatingCity
  }
init :: JourneyLeg leg m => JourneyInitData leg -> m Journey
init journeyReq = do
  journey = mkJourney journeyReq
  JQ.create journey -- it will internally create journey leg entries as well

getJourney :: Id Journey -> m Journey
getJourney id = JQ.findById id

addLeg :: JourneyLeg leg m => leg -> m ()
addLeg leg = JL.search leg -- output could be the search request

getCurrentLeg :: JourneyLeg leg m => Journey -> m leg
getCurrentLeg
  -- get the current leg based on status
  -- loop through all legs and based on getState
  journeyLegs <- getAllLegs journeyId
  let currentLeg = find (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return currentLeg

getRemainingLegs :: JourneyLeg leg m => Id Journey -> m [leg]
getRemainingLegs journeyId = do
  -- get the remaining legs
  journeyLegs <- getAllLegs journeyId
  let remainingLegs = filter (\leg -> notElem leg.status [completedStatus]) journeyLegs
  return remainingLegs
--   filter -> based on status

-- createLeg :: RequiedData (OTP response for each leg)
deleteLeg :: JourneyLeg leg m => leg -> m ()
  JL.cancel leg
  -- remove it from the journey mark it deleted/skipped

updateLeg :: JourneyLeg leg m => UpdateJourneyLeg -> leg -> m ()
  -- update the leg
  -- update it in the journey

skipJourney :: Journey -> [leg] -> m ()
-- skipJourney journey
    -- getRemainingLegs
    map update [leg]
    -- @@ call cancel for current leg

endJourney :: Journey -> m ()
endJourney
-- if last leg then update leg
-- loop through and delete/update legs and journey as required
-- call leg level cancel

startJourney :: Id Journey -> ConfirmReq -> m () -- confirm request
startJourney journeyId = do
  -- journey <- getJourney journeyId
  allLegs <- getAllLegs journeyId
  mapM JL.confirm allLegs


getAllLegs :: Id Journey -> m ()
getAllLegs journeyId = do
  searchRequests <- findAllByJourneyId journeyId -- in searchRequest
  frfsSearchRequests <- findAllByJourneyId journeyId -- in FRFS search
  let legs = sortBy order $ transformSearchReqToJourneyLeg searchRequests  <> transformFRFSSearchReqToJourneyLegfrfs SearchRequests
  return legs

replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
replaceLeg journey oldLegs newLeg =
  forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
  forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg





-- Journey Module (utils function)
-- replace auto with taxi / generic name
replaceCurrentWithAuto :: Journey -> VehicleType -> m () -- add vehicleType
replaceCurrentWithAuto journey vehicleType = do
  currentLeg <- getCurrentLeg journey
  let currentState = getState currentLeg
  let autoLeg = mkAutoLeg currentState.currentPosition currentLeg vehicleType
  replaceLeg journey [currentLeg] autoLeg

replaceAllWithAuto :: Journey -> VehicleType -> m () -- add vehicleType
replaceAllWithAuto journey vehicleType = do
  currentLeg <- getCurrentLeg journey
  let currentState = getState currentLeg
  legs <- getRemaningLegs journey
  let lastLegInfo = get legs[length legs - 1]
  let autoLeg = mkAutoLeg currentState.currentPosition lastLegInfo vehicleType
  replaceLeg journey legs autoLeg

continueWithAuto :: Journey -> m ()
continueWithAuto journey = do
  currentAutoLeg <- getCurrentLeg journey
  legs <- getRemaningLegs journey
  let lastLegInfo = get legs[length legs - 1]
  let autoLeg = currentAutoLeg { dropPoint = lastLegInfo.endLocation}
  extendLeg journey legs autoLeg



-- what to be shown in case of skip on UI

updateJourney :: Journey -> Id Journey -> m ()-- to update journey data


recalculate :: Journey -> m ()
let legs = getRemainingLegs
-- for now nothing just call replace with auto

-- completeCurrentLeg :: Journey -> m ()
