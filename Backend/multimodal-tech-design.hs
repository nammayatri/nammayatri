module Domain.Types.JourneyLegs where

data JourneyLegStatus =
    InPlan
  -- | Booking
  -- | RetryBooking
  | Assigning
  -- | ReAssigning
  | Booked
  | OnTime
  | AtRiskOfMissing
  | Departed
  | Missed
  | Delayed
  | Arriving
  -- | Skipped
  | Ongoing
  | Finishing
  | Cancelled
  | Completed
  deriving (Eq, Show)

completedStatus :: [JourneyLegStatus]
completedStatus = [Cancelled, Completed]

data ServiceStatus = Pending | Failed | Confirmed | Verified | Cancelled deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RideStatus = UPCOMING | NEW | INPROGRESS | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)


-- Mapper functions
mapRideStatusToJourneyLegStatus :: RideStatus -> JourneyLegStatus
mapRideStatusToJourneyLegStatus status = case status of
    UPCOMING    -> InPlan
    NEW         -> Booked
    INPROGRESS  -> Ongoing
    COMPLETED   -> Completed
    CANCELLED   -> Cancelled

mapServiceStatusToJourneyLegStatus :: ServiceStatus -> JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
    Pending   -> InPlan -- Indicates the service is yet to start planning.
    Failed    -> FailedStatus -- Indicates a failure in the service.
    Confirmed -> Booked -- Indicates the service has been confirmed/booked.
    Verified  -> Assigning -- Indicates the service is verified and assigning resources.
    Cancelled -> Cancelled -- Indicates the service has been cancelled.

data JourneyLegState = {
  status :: JourneyLegStatus,
  currentPosition :: LatLong | Station
}

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

data WalkSearchData = WalkSearchData
  { fromLocation :: Location
  , stops :: [Location]
  , merchantId :: Id Merchant
  ... other things
  }

data TaxiConfirmData = TaxiConfirmData
  { skipBooking :: Bool,
    journeyLegOrder :: Int
  }

data WalkLegRequest = WalkLegRequestSearch MultiModalLeg WalkSearchData | WalkLegRequestConfirm WalkLegConfirmRequest | WalkLegRequestCancel WalkLegCancelRequest | WalkLegRequestUpdate WalkLegUpdateRequest
data TaxiLegRequest = TaxiLegRequestSearch MultiModalLeg TaxiSearchData | TaxiLegRequestConfirm TaxiConfirmData
data MetroLegRequest = MetroLegRequestSearch MultiModalLeg MetroSearchData | MetroLegRequestConfirm MetroConfirmData
data BusLegRequest = BusLegRequestSearch MultiModalLeg BusSearchData | BusLegRequestConfirm BusConfirmData

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

instance JourneyLeg WalkLeg m where
  search (WalkLegRequestSearch multimodalLeg walkLegSearchData) = do
    WL.create $ mkWalkLegSearch multimodalLeg walkLegSearchData
  confirm (WalkLeg _legData) = return () -- return nothing
  update (WalkLeg $ Update WalkLegUpdateRequest) JourneyLegStatus = return ()
    -- WalkLegUpdateRequest :: id, mbFromLocation, mbtoLocation, mbStartTime
  cancel (WalkLeg $ Cancel WalkLegCancelRequest) JourneyLegStatus = return ()
    -- update JourneyLegStatus: Cancelled
    WalkLegCancelRequest :: id
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
  update (TaxiLeg $ Update TaxiLegUpdateRequest) = return ()
    -- not possible for JourneyLegStatus = Skipped | Finishing | Cancelled | Completed
    -- TaxiLegUpdateRequest :: id, mbFromLocation, mbtoLocation, mbStartTime, mbVehicleType
    -- Just vehicleType -> switchTaxi      -- not possible for OnGoing
    -- Just fromLocation -> editPickup       -- not possible for OnGoing
    -- Just toLocation -> editDestination
    -- Just startTime -> if nightTime -> recompute price, scheduledRides -> change time
  cancel (TaxiLeg _legData) = return ()
    -- call cancelV2
    -- update JourneyLegStatus: Cancelled
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
  update (BusLeg _legData) = return ()
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
  cancel (MetroLeg _legData) = return ()
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