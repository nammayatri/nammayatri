module Lib.JourneyModule.Base where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Domain.Action.UI.Location as DLoc
import Domain.Action.UI.Ride as DRide
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import Domain.Types.Extra.Ride as DRide
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationAddress as LA
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Trip as DTrip
import Environment
import Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import Kernel.External.MultiModal.Interface.Types
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Distance
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Bus ()
import qualified Lib.JourneyLeg.Interface as JLI
import Lib.JourneyLeg.Metro ()
import Lib.JourneyLeg.Subway ()
import Lib.JourneyLeg.Taxi ()
import qualified Lib.JourneyLeg.Types as JL
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Subway
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyLeg.Walk ()
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JL
import Lib.JourneyModule.Utils
import SharedLogic.Search
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchOpCity
import Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Journey as JQ
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Maps as Maps

init ::
  JL.GetFareFlow m r =>
  JL.JourneyInitData ->
  m (Maybe DJourney.Journey)
init journeyReq = do
  journeyId <- Common.generateGUID
  riderConfig <- QRC.findByMerchantOperatingCityId journeyReq.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist journeyReq.merchantOperatingCityId.getId)
  mbTotalFares <-
    mapWithIndex
      ( \idx leg -> do
          let travelMode = convertMultiModalModeToTripMode leg.mode (distanceToMeters leg.distance) journeyReq.maximumWalkDistance
          mbTotalLegFare <- JLI.getFare journeyReq.merchantId journeyReq.merchantOperatingCityId leg travelMode
          if riderConfig.multimodalTesting
            then do
              journeyLeg <- JL.mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance mbTotalLegFare
              QJourneyLeg.create journeyLeg
            else do
              whenJust mbTotalLegFare $ \fare -> do
                journeyLeg <- JL.mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance (Just fare)
                QJourneyLeg.create journeyLeg
          return mbTotalLegFare
      )
      journeyReq.legs
  logDebug $ "[Multimodal - Legs]" <> show mbTotalFares
  if not riderConfig.multimodalTesting && (any isNothing mbTotalFares)
    then do return Nothing
    else do
      journey <- JL.mkJourney journeyReq.personId journeyReq.startTime journeyReq.endTime journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyReq.legs journeyReq.maximumWalkDistance
      QJourney.create journey
      logDebug $ "journey for multi-modal: " <> show journey
      return $ Just journey

getJourney :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m DJourney.Journey
getJourney id = JQ.findByPrimaryKey id >>= fromMaybeM (JourneyNotFound id.getId)

getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m [DJourneyLeg.JourneyLeg]
getJourneyLegs journeyId = do
  legs <- QJourneyLeg.findAllByJourneyId journeyId
  let filteredLegs = filter (\leg -> leg.isDeleted == Just False || leg.isDeleted == Nothing) legs
  return $ sortBy (comparing (.sequenceNumber)) filteredLegs

getAllLegsInfo ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getAllLegsInfo journeyId = do
  allLegsRawData <- getJourneyLegs journeyId
  allLegsInfo <-
    allLegsRawData `forM` \leg -> do
      case leg.legSearchId of
        Just legSearchIdText -> do
          let legSearchId = Id legSearchIdText
          case leg.mode of
            DTrip.Taxi -> JL.getInfo $ TaxiLegRequestGetInfo $ TaxiLegRequestGetInfoData {searchId = cast legSearchId}
            DTrip.Walk -> JL.getInfo $ WalkLegRequestGetInfo $ WalkLegRequestGetInfoData {walkLegId = cast legSearchId}
            DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare, distance = leg.distance, duration = leg.duration}
            DTrip.Subway -> JL.getInfo $ SubwayLegRequestGetInfo $ SubwayLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare, distance = leg.distance, duration = leg.duration}
            DTrip.Bus -> JL.getInfo $ BusLegRequestGetInfo $ BusLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare, distance = leg.distance, duration = leg.duration}
        Nothing -> throwError $ InvalidRequest ("LegId null for Mode: " <> show leg.mode)
  return $ allLegsInfo

getAllLegsStatus ::
  JL.GetStateFlow m r c =>
  DJourney.Journey ->
  m [JL.JourneyLegState]
getAllLegsStatus journey = do
  allLegsRawData <- getJourneyLegs journey.id
  riderLastPoints <- getLastThreePoints journey.id
  (_, allLegsState) <- foldlM (processLeg riderLastPoints) (True, []) allLegsRawData
  when ((all (\st -> st.status `elem` [JL.Completed, JL.Skipped, JL.Cancelled]) allLegsState) && journey.status /= DJourney.CANCELLED) $ do
    updateJourneyStatus journey DJourney.FEEDBACK_PENDING
  return allLegsState
  where
    processLeg ::
      JL.GetStateFlow m r c =>
      [APITypes.RiderLocationReq] ->
      (Bool, [JL.JourneyLegState]) ->
      DJourneyLeg.JourneyLeg ->
      m (Bool, [JL.JourneyLegState])
    processLeg riderLastPoints (isLastCompleted, legsState) leg = do
      case leg.legSearchId of
        Just legSearchIdText -> do
          let legSearchId = Id legSearchIdText
          legState <-
            case leg.mode of
              DTrip.Taxi -> JL.getState $ TaxiLegRequestGetState $ TaxiLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Walk -> JL.getState $ WalkLegRequestGetState $ WalkLegRequestGetStateData {walkLegId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Metro -> JL.getState $ MetroLegRequestGetState $ MetroLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Subway -> JL.getState $ SubwayLegRequestGetState $ SubwayLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
              DTrip.Bus -> JL.getState $ BusLegRequestGetState $ BusLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastCompleted}
          return (legState.status == JL.Completed, legsState <> [legState])
        Nothing -> throwError $ InvalidRequest ("LegId null for Mode: " <> show leg.mode)

startJourney ::
  (JL.ConfirmFlow m r c, JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Maybe Int ->
  Id DJourney.Journey ->
  m ()
startJourney forcedBookedLegOrder journeyId = do
  allLegs <- getAllLegsInfo journeyId
  mapM_ (\leg -> JLI.confirm (Just leg.order == forcedBookedLegOrder) leg) allLegs

addAllLegs ::
  ( JL.SearchRequestFlow m r c
  ) =>
  Id DJourney.Journey ->
  m ()
addAllLegs journeyId = do
  journey <- getJourney journeyId
  journeyLegs <- getJourneyLegs journeyId
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
  toLocation <- parentSearchReq.toLocation & fromMaybeM (InvalidRequest "To location nothing for parent search request")
  forM_ (traverseWithTriplets journeyLegs) $ \(mbPrevJourneyLeg, journeyLeg, mbNextJourneyLeg) -> do
    when (isNothing journeyLeg.legSearchId) $ do
      -- In case of retry of this function, if search has already triggered then it will not do it again
      searchResp <-
        case journeyLeg.mode of
          DTrip.Taxi -> do
            let originAddress = mkAddress (mbPrevJourneyLeg >>= (.toStopDetails)) parentSearchReq.fromLocation.address
            let destinationAddress = mkAddress (mbNextJourneyLeg >>= (.fromStopDetails)) toLocation.address
            addTaxiLeg parentSearchReq journeyLeg originAddress destinationAddress
          DTrip.Metro -> do
            addMetroLeg parentSearchReq journeyLeg
          DTrip.Subway -> do
            addSubwayLeg parentSearchReq journeyLeg
          DTrip.Walk -> do
            let originAddress = mkAddress (mbPrevJourneyLeg >>= (.toStopDetails)) parentSearchReq.fromLocation.address
            let destinationAddress = mkAddress (mbNextJourneyLeg >>= (.fromStopDetails)) toLocation.address
            addWalkLeg parentSearchReq journeyLeg originAddress destinationAddress
          DTrip.Bus -> do
            addBusLeg parentSearchReq journeyLeg
      QJourneyLeg.updateLegSearchId (Just searchResp.id) journeyLeg.id
  where
    traverseWithTriplets :: [a] -> [(Maybe a, a, Maybe a)]
    traverseWithTriplets [] = []
    traverseWithTriplets [x] = [(Nothing, x, Nothing)] -- Single element case
    traverseWithTriplets xs = go Nothing xs
      where
        go _ [] = []
        go prev [x] = [(prev, x, Nothing)] -- Last element case
        go prev (x : y : rest) = (prev, x, Just y) : go (Just x) (y : rest)

    mkAddress :: Maybe MultiModalStopDetails -> LA.LocationAddress -> LA.LocationAddress
    mkAddress Nothing parentAddress = parentAddress
    mkAddress (Just stopDetails) _ =
      LA.LocationAddress
        { street = Nothing,
          door = Nothing,
          city = Nothing,
          state = Nothing,
          country = Nothing,
          building = Nothing,
          areaCode = Nothing,
          area = stopDetails.name,
          ward = Nothing,
          placeId = Nothing,
          instructions = Nothing,
          title = stopDetails.name,
          extras = Nothing
        }

addTaxiLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  LA.LocationAddress ->
  LA.LocationAddress ->
  m JL.SearchResponse
addTaxiLeg parentSearchReq journeyLeg originAddress destinationAddress = do
  let startLocation = JL.mkSearchReqLocation originAddress journeyLeg.startLocation
  let endLocation = JL.mkSearchReqLocation destinationAddress journeyLeg.endLocation
  let taxiSearchReq = mkTaxiSearchReq startLocation [endLocation]
  JL.search taxiSearchReq
  where
    mkTaxiSearchReq :: SearchReqLocation -> [SearchReqLocation] -> TaxiLegRequest
    mkTaxiSearchReq origin stops =
      TaxiLegRequestSearch $
        TaxiLegRequestSearchData
          { journeyLegData = journeyLeg,
            ..
          }

addWalkLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  LA.LocationAddress ->
  LA.LocationAddress ->
  m JL.SearchResponse
addWalkLeg parentSearchReq journeyLeg originAddress destinationAddress = do
  let startLocation = JL.mkSearchReqLocation originAddress journeyLeg.startLocation
  let endLocation = JL.mkSearchReqLocation destinationAddress journeyLeg.endLocation
  let walkSearchReq = mkWalkSearchReq startLocation endLocation
  JL.search walkSearchReq
  where
    mkWalkSearchReq :: SearchReqLocation -> SearchReqLocation -> WalkLegRequest
    mkWalkSearchReq origin destination =
      WalkLegRequestSearch $
        WalkLegRequestSearchData
          { journeyLegData = journeyLeg,
            ..
          }

addMetroLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addMetroLeg parentSearchReq journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById parentSearchReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound parentSearchReq.merchantOperatingCityId.getId)
  let metroSearchReq = mkMetroLegReq merchantOperatingCity.city
  JL.search metroSearchReq
  where
    mkMetroLegReq city = do
      MetroLegRequestSearch $
        MetroLegRequestSearchData
          { quantity = 1,
            personId = parentSearchReq.riderId,
            merchantId = parentSearchReq.merchantId,
            city,
            journeyLeg
          }

addSubwayLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addSubwayLeg parentSearchReq journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById parentSearchReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound parentSearchReq.merchantOperatingCityId.getId)
  let subwaySearchReq = mkSubwayLegReq merchantOperatingCity.city
  JL.search subwaySearchReq
  where
    mkSubwayLegReq city = do
      SubwayLegRequestSearch $
        SubwayLegRequestSearchData
          { quantity = 1,
            personId = parentSearchReq.riderId,
            merchantId = parentSearchReq.merchantId,
            city,
            journeyLeg
          }

addBusLeg ::
  JL.SearchRequestFlow m r c =>
  SearchRequest.SearchRequest ->
  DJourneyLeg.JourneyLeg ->
  m JL.SearchResponse
addBusLeg parentSearchReq journeyLeg = do
  merchantOperatingCity <- QMerchOpCity.findById parentSearchReq.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound parentSearchReq.merchantOperatingCityId.getId)
  let busSearchReq = mkBusLegReq merchantOperatingCity.city
  JL.search busSearchReq
  where
    mkBusLegReq city = do
      BusLegRequestSearch $
        BusLegRequestSearchData
          { quantity = 1,
            personId = parentSearchReq.riderId,
            merchantId = parentSearchReq.merchantId,
            city,
            journeyLeg
          }

getCurrentLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m (Maybe JL.LegInfo)
getCurrentLeg journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let currentLeg = find (\leg -> leg.status == JL.Ongoing) journeyLegs
  return currentLeg

getRemainingLegs ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = filter cancellableStatus journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
  return remainingLegs

cancellableStatus :: JL.LegInfo -> Bool
cancellableStatus leg = if leg.travelMode == DTrip.Walk then not (leg.status `elem` JL.cannotCancelWalkStatus) else not (leg.status `elem` JL.cannotCancelStatus)

getUnifiedQR :: [JL.LegInfo] -> UTCTime -> Maybe JL.UnifiedTicketQR
getUnifiedQR legs now = do
  let bookings = mapMaybe getTickets (filter (\leg -> leg.travelMode `elem` [DTrip.Metro, DTrip.Bus, DTrip.Subway]) legs)
  let cmrlBookings = [b | (provider, b) <- bookings, provider == providerToText JL.CMRL]
  let mtcBookings = [b | (provider, b) <- bookings, provider == providerToText JL.MTC]
  if null cmrlBookings && null mtcBookings
    then Nothing
    else
      Just $
        JL.UnifiedTicketQR
          { version = "1.0",
            txnId = "nammayatri-test-N62dNNcFc8-1",
            createdAt = now,
            cmrl = cmrlBookings,
            mtc = mtcBookings
          }

providerToText :: JL.Provider -> Text
providerToText JL.CMRL = "Chennai Metro Rail Limited"
providerToText JL.MTC = "Buses"

getTickets :: JL.LegInfo -> Maybe (Text, JL.BookingData)
getTickets leg =
  leg.pricingId >>= \bookingId -> do
    case leg.legExtraInfo of
      JL.Metro info -> processTickets JL.CMRL info.tickets info.providerName bookingId
      JL.Bus info -> processTickets JL.MTC info.tickets info.providerName bookingId
      _ -> Nothing
  where
    processTickets :: JL.Provider -> Maybe [Text] -> Maybe Text -> Text -> Maybe (Text, JL.BookingData)
    processTickets expectedProvider mbTickets mbProviderName bookingId = do
      tickets <- mbTickets
      provider <- mbProviderName
      if provider == providerToText expectedProvider
        then
          Just
            ( provider,
              JL.BookingData
                { bookingId = bookingId,
                  isRoundTrip = False,
                  ticketData = tickets
                }
            )
        else Nothing

cancelLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  JL.LegInfo ->
  SCR.CancellationReasonCode ->
  Bool ->
  m ()
cancelLeg journeyLeg cancellationReasonCode isSkipped = do
  isCancellable <- checkIfCancellable journeyLeg
  unless isCancellable $
    throwError $ InvalidRequest $ "Cannot cancel leg for: " <> show journeyLeg.travelMode
  case journeyLeg.travelMode of
    DTrip.Taxi ->
      JL.cancel $
        TaxiLegRequestCancel
          TaxiLegRequestCancelData
            { searchRequestId = Id journeyLeg.searchId,
              reasonCode = cancellationReasonCode,
              additionalInfo = Nothing,
              reallocate = Nothing,
              blockOnCancellationRate = Nothing,
              cancellationSource = SBCR.ByUser,
              isSkipped
            }
    DTrip.Walk ->
      JL.cancel $
        WalkLegRequestCancel
          WalkLegRequestCancelData
            { walkLegId = Id journeyLeg.searchId
            }
    DTrip.Metro ->
      JL.cancel $
        MetroLegRequestCancel
          MetroLegRequestCancelData
            { searchId = Id journeyLeg.searchId,
              cancellationType = Spec.CONFIRM_CANCEL,
              isSkipped
            }
    DTrip.Subway -> JL.cancel $ SubwayLegRequestCancel SubwayLegRequestCancelData
    DTrip.Bus ->
      JL.cancel $
        BusLegRequestCancel
          BusLegRequestCancelData
            { searchId = Id journeyLeg.searchId,
              cancellationType = Spec.CONFIRM_CANCEL,
              isSkipped
            }
  return ()

cancelRemainingLegs ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  m ()
cancelRemainingLegs journeyId = do
  remainingLegs <- getRemainingLegs journeyId
  forM_ remainingLegs $ \leg -> do
    isCancellable <- checkIfCancellable leg
    unless isCancellable $
      throwError $ InvalidRequest $ "Cannot cancel leg for leg: " <> show leg.travelMode
  results <-
    forM remainingLegs $ \leg -> do
      try @_ @SomeException (if leg.skipBooking then return () else cancelLeg leg (SCR.CancellationReasonCode "") False)
  let failures = [e | Left e <- results]
  unless (null failures) $
    throwError $ InvalidRequest $ "Failed to cancel some legs: " <> show failures

skipLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  Int ->
  m ()
skipLeg journeyId legOrder = do
  allLegs <- getAllLegsInfo journeyId
  skippingLeg <- find (\leg -> leg.order == legOrder) allLegs & fromMaybeM (InvalidRequest $ "Leg not found: " <> show legOrder)
  unless (cancellableStatus skippingLeg) $
    throwError $ InvalidRequest $ "Leg cannot be skipped in status: " <> show skippingLeg.status

  cancelLeg skippingLeg (SCR.CancellationReasonCode "") True

checkIfCancellable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  JL.LegInfo ->
  m Bool
checkIfCancellable journeyLeg = do
  isCancellableResult <- case journeyLeg.travelMode of
    DTrip.Taxi ->
      JL.isCancellable $
        TaxiLegRequestIsCancellable
          TaxiLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId
            }
    DTrip.Walk ->
      JL.isCancellable $
        WalkLegRequestIsCancellable
          WalkLegRequestIsCancellableData
            { walkLegId = Id journeyLeg.searchId
            }
    DTrip.Metro ->
      JL.isCancellable $
        MetroLegRequestIsCancellable
          MetroLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId
            }
    DTrip.Subway -> JL.isCancellable $ SubwayLegRequestIsCancellable SubwayLegRequestIsCancellableData
    DTrip.Bus ->
      JL.isCancellable $
        BusLegRequestIsCancellable
          BusLegRequestIsCancellableData
            { searchId = Id journeyLeg.searchId
            }
  return isCancellableResult.canCancel

canBeSwitched ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  JL.LegInfo ->
  DTrip.MultimodalTravelMode ->
  m Bool
canBeSwitched legToBeSwitched newMode = do
  let currentMode = legToBeSwitched.travelMode
  case (currentMode, newMode) of
    (_, DTrip.Metro) -> return False
    (_, DTrip.Bus) -> return False
    (DTrip.Bus, DTrip.Taxi) -> return $ legToBeSwitched.status /= JL.Ongoing
    (DTrip.Metro, DTrip.Taxi) -> return $ legToBeSwitched.status /= JL.Ongoing
    (DTrip.Walk, DTrip.Taxi) -> return True
    (DTrip.Taxi, DTrip.Walk) -> do
      case legToBeSwitched.estimatedDistance of
        Just distance ->
          if getMeters (distanceToMeters distance) <= 2000
            then do return True
            else do throwError $ InvalidRequest "Can't switch to walk if distance is more than 2km, skip the ride instead"
        Nothing -> return True
    _ -> return False

updateJourneyStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  DJourney.Journey ->
  DJourney.JourneyStatus ->
  m ()
updateJourneyStatus journey newStatus = do
  when (newStatus > journey.status) $
    QJourney.updateStatus newStatus journey.id

isExtendable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  [JL.LegInfo] ->
  JL.LegInfo ->
  DTrip.MultimodalTravelMode ->
  m Bool
isExtendable allLegs currentLegInfo newMode = do
  let previousLegInfo = find (\leg -> leg.order == (currentLegInfo.order - 1)) allLegs
      nextLegInfo = find (\leg -> leg.order == (currentLegInfo.order + 1)) allLegs
  let startLeg =
        case previousLegInfo of
          Just prevLeg | prevLeg.travelMode == newMode -> prevLeg
          _ -> currentLegInfo
      endLeg =
        case nextLegInfo of
          Just nextLeg | nextLeg.travelMode == newMode -> nextLeg
          _ -> currentLegInfo
   in return (startLeg.order /= currentLegInfo.order || endLeg.order /= currentLegInfo.order)

createJourneyLegFromCancelledLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  DJourneyLeg.JourneyLeg ->
  DTrip.MultimodalTravelMode ->
  Maybe Maps.LatLngV2 ->
  m DJourneyLeg.JourneyLeg
createJourneyLegFromCancelledLeg journeyLeg newMode startLoc = do
  now <- getCurrentTime
  journeyLegId <- generateGUID
  startLocation <- return $ fromMaybe journeyLeg.startLocation startLoc
  (newDistance, newDuration) <-
    case newMode of
      DTrip.Walk -> do
        merchantId <- journeyLeg.merchantId & fromMaybeM (InvalidRequest $ "MerchantId not found for journeyLegId: " <> journeyLeg.id.getId)
        merchantOperatingCityId <- journeyLeg.merchantOperatingCityId & fromMaybeM (InvalidRequest $ "MerchantOperatingCityId not found for journeyLegId: " <> journeyLeg.id.getId)
        newDistanceAndDuration <-
          Maps.getMultimodalWalkDistance merchantId merchantOperatingCityId $
            Maps.GetDistanceReq
              { origin = LatLong {lat = startLocation.latitude, lon = startLocation.longitude},
                destination = LatLong {lat = journeyLeg.endLocation.latitude, lon = journeyLeg.endLocation.longitude},
                travelMode = Just Maps.FOOT,
                sourceDestinationMapping = Nothing,
                distanceUnit = Meter
              }
        return (Just newDistanceAndDuration.distanceWithUnit, Just newDistanceAndDuration.duration)
      _ -> return (journeyLeg.distance, journeyLeg.duration)

  return $
    DJourneyLeg.JourneyLeg
      { agency = Nothing,
        distance = newDistance,
        duration = newDuration,
        endLocation = journeyLeg.endLocation,
        fromArrivalTime = Nothing,
        fromDepartureTime = Nothing,
        fromStopDetails = Nothing,
        id = journeyLegId,
        journeyId = journeyLeg.journeyId,
        mode = newMode,
        routeDetails = Nothing,
        sequenceNumber = journeyLeg.sequenceNumber,
        startLocation = startLocation,
        toArrivalTime = Nothing,
        toDepartureTime = journeyLeg.toDepartureTime,
        toStopDetails = journeyLeg.toStopDetails,
        estimatedMinFare = Nothing,
        estimatedMaxFare = Nothing,
        merchantId = journeyLeg.merchantId,
        merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        isDeleted = Just False,
        legSearchId = Nothing,
        isSkipped = Just False
      }

extendLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  JL.ExtendLegStartPoint ->
  Maybe DLocation.LocationAPIEntity ->
  Maybe Int ->
  JL.GetFareResponse ->
  Distance ->
  Seconds ->
  m ()
extendLeg journeyId startPoint mbEndLocation mbEndLegOrder fare newDistance newDuration = do
  journey <- getJourney journeyId
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for searchId: " <> show parentSearchReq.id.getId) parentSearchReq.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation
  case startPoint of
    JL.StartLegOrder startLegOrder -> do
      allLegs <- getAllLegsInfo journeyId
      currentLeg <- find (\leg -> leg.order == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
      legsToCancel <-
        case mbEndLegOrder of
          Just endLegOrder -> return $ filter (\leg -> leg.order >= startLegOrder && leg.order < endLegOrder) allLegs
          Nothing -> return $ filter (\leg -> leg.order >= startLegOrder) allLegs
      checkIfRemainingLegsAreCancellable legsToCancel
      forM_ legsToCancel $ \leg -> do
        cancelLeg leg (SCR.CancellationReasonCode "") False
      (newOriginLat, newOriginLon) <- getNewOriginLatLon currentLeg.legExtraInfo
      let leg = mkMultiModalLeg newDistance newDuration MultiModalTypes.Unspecified newOriginLat newOriginLon endLocation.lat endLocation.lon
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg startLegOrder leg currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance (Just fare)
      QJourneyLeg.create journeyLeg
      addAllLegs journeyId
      return ()
    JL.StartLocation startlocation -> do
      currentLeg <- getCurrentLeg journeyId >>= fromMaybeM (InternalError "Current leg not found")
      case currentLeg.travelMode of
        DTrip.Taxi -> do
          booking <- QBooking.findByTransactionId currentLeg.searchId >>= fromMaybeM (BookingNotFound $ "transactionId:-" <> currentLeg.searchId)
          ride <- QRide.findByRBId booking.id >>= fromMaybeM (InvalidRequest $ "No Ride present for booking" <> booking.id.getId)
          let editLocReq =
                DRide.EditLocationReq
                  { origin = Nothing,
                    destination = Just $ DRide.EditLocation {gps = LatLong {lat = endLocation.lat, lon = endLocation.lon}, address = getAddress endLocation}
                  }
          void $ DRide.editLocation ride.id (currentLeg.personId, currentLeg.merchantId) editLocReq -- handle case if driver declines
          journeyLeg <- QJourneyLeg.findByLegSearchId (Just currentLeg.searchId) >>= fromMaybeM (InvalidRequest $ "JourneyLeg not found for searchId: " <> currentLeg.searchId)
          QJourneyLeg.updateAfterEditLocation (Just newDuration) (Just newDistance) (LatLngV2 {latitude = endLocation.lat, longitude = endLocation.lon}) journeyLeg.id
          cancelRequiredLegs
        DTrip.Walk -> do
          cancelRequiredLegs
          let leg = mkMultiModalLeg newDistance newDuration MultiModalTypes.Unspecified startlocation.lat startlocation.lon endLocation.lat endLocation.lon
          riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
          journeyLeg <- JL.mkJourneyLeg currentLeg.order leg currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance (Just fare)
          QJourneyLeg.create journeyLeg
          addAllLegs journeyId
          return ()
        _ -> do
          throwError $ InvalidRequest ("Cannot extend leg for mode: " <> show currentLeg.travelMode)
  where
    getAddress DLocation.LocationAPIEntity {..} = LA.LocationAddress {..}
    cancelRequiredLegs = do
      case mbEndLegOrder of
        Nothing -> cancelRemainingLegs journeyId
        Just endLegOrder -> do
          remainingLegs <- getRemainingLegs journeyId
          let legsToCancel = filter (\leg -> leg.order < endLegOrder) remainingLegs
          checkIfRemainingLegsAreCancellable legsToCancel
          mapM_ (\leg -> cancelLeg leg (SCR.CancellationReasonCode "") False) legsToCancel
    getNewOriginLatLon legExtraInfo =
      case legExtraInfo of
        JL.Walk info -> return (info.origin.lat, info.origin.lon)
        JL.Taxi info -> return (info.origin.lat, info.origin.lon)
        JL.Metro info -> getLatLon "Metro" info.originStop.lat info.originStop.lon
        JL.Subway info -> getLatLon "Subway" info.originStop.lat info.originStop.lon
        JL.Bus info -> getLatLon "Bus" info.originStop.lat info.originStop.lon
    getLatLon label mLat mLon = do
      lat <- fromMaybeM (InvalidRequest $ label <> " latitude not found") mLat
      lon <- fromMaybeM (InvalidRequest $ label <> " longitude not found") mLon
      return (lat, lon)
    mkMultiModalLeg distance duration mode originLat originLon destLat destLon =
      MultiModalTypes.MultiModalLeg
        { distance,
          duration,
          polyline = Polyline {encodedPolyline = ""},
          mode,
          startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
          fromStopDetails = Nothing,
          toStopDetails = Nothing,
          routeDetails = Nothing,
          agency = Nothing,
          fromArrivalTime = Nothing,
          fromDepartureTime = Nothing,
          toArrivalTime = Nothing,
          toDepartureTime = Nothing
        }

checkIfRemainingLegsAreCancellable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  [JL.LegInfo] ->
  m ()
checkIfRemainingLegsAreCancellable legsToCancel = do
  forM_ legsToCancel $ \leg -> do
    isCancellable <- checkIfCancellable leg
    unless isCancellable $
      throwError $ InvalidRequest $ "Cannot cancel leg for leg order: " <> show leg.order

checkIfAllLegsCancellable ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  [JL.LegInfo] ->
  m Bool
checkIfAllLegsCancellable remainingLegs = do
  cancellableResults <- mapM checkIfCancellable remainingLegs
  return (and cancellableResults)

getExtendLegs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  Int ->
  m [JL.LegInfo]
getExtendLegs journeyId legOrder = do
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = filter (\leg -> notElem (leg.status) JL.cannotCancelWalkStatus && leg.order <= legOrder) journeyLegs
  return remainingLegs

extendLegEstimatedFare ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  Id DJourney.Journey ->
  JL.ExtendLegStartPoint ->
  Maybe DLocation.LocationAPIEntity ->
  Maybe Int ->
  m APITypes.ExtendLegGetFareResp
extendLegEstimatedFare journeyId startPoint mbEndLocation legOrder = do
  journey <- getJourney journeyId
  allLegs <- getAllLegsInfo journeyId
  remainingLegs <- case legOrder of
    Just order -> getExtendLegs journeyId order
    Nothing -> getRemainingLegs journeyId

  currentLeg <- case startPoint of
    JL.StartLegOrder startLegOrder -> find (\leg -> leg.order == startLegOrder) allLegs & fromMaybeM (InvalidRequest $ "Cannot find leg with order: " <> show startLegOrder)
    JL.StartLocation _ -> getCurrentLeg journeyId >>= fromMaybeM (InternalError "Current leg not found")

  isLegsCancellable <- checkIfAllLegsCancellable remainingLegs
  if isLegsCancellable
    then do
      parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
      endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for searchId: " <> show parentSearchReq.id.getId) parentSearchReq.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation

      startLocation <- getStartLocation startPoint remainingLegs
      distResp <-
        Maps.getDistance currentLeg.merchantId currentLeg.merchantOperatingCityId $
          Maps.GetDistanceReq
            { origin = startLocation,
              destination = getEndLocation endLocation,
              travelMode = Just Maps.CAR,
              sourceDestinationMapping = Nothing,
              distanceUnit = Meter
            }
      let distance = convertMetersToDistance Meter distResp.distance
      let multiModalLeg = mkMultiModalLeg distance distResp.duration MultiModalTypes.Unspecified startLocation.lat startLocation.lon endLocation.lat endLocation.lon
      estimatedFare <- JLI.getFare currentLeg.merchantId currentLeg.merchantOperatingCityId multiModalLeg DTrip.Taxi
      return $
        APITypes.ExtendLegGetFareResp
          { totalFare = estimatedFare,
            distance = distance,
            duration = distResp.duration
          }
    else throwError (InvalidRequest "Legs are not cancellable")
  where
    getEndLocation location =
      LatLong
        { lat = location.lat,
          lon = location.lon
        }
    getStartLocation (JL.StartLocation startloc) _ = pure $ LatLong {lat = startloc.lat, lon = startloc.lon}
    getStartLocation (JL.StartLegOrder startLegOrder) legs = do
      startLeg <- find (\leg -> leg.order == startLegOrder) legs & fromMaybeM (InvalidRequest ("Journey Leg not Present" <> show startLegOrder))
      case startLeg.legExtraInfo of
        JL.Taxi info -> mkLatLng info.origin
        JL.Walk info -> mkLatLng info.origin
        JL.Metro info -> mkLatLngFromFRFS info.originStop
        JL.Bus info -> mkLatLngFromFRFS info.originStop
        JL.Subway info -> mkLatLngFromFRFS info.originStop

    mkLatLng originLocation = pure $ LatLong {lat = originLocation.lat, lon = originLocation.lon}
    mkLatLngFromFRFS startLoc = do
      latitude <- startLoc.lat & fromMaybeM (InvalidRequest "Start location not Found")
      longitude <- startLoc.lon & fromMaybeM (InvalidRequest "Start location not Found")
      return $ LatLong {lat = latitude, lon = longitude}

    mkMultiModalLeg distance duration mode originLat originLon destLat destLon =
      MultiModalTypes.MultiModalLeg
        { distance,
          duration,
          polyline = Polyline {encodedPolyline = ""},
          mode,
          startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
          fromStopDetails = Nothing,
          toStopDetails = Nothing,
          routeDetails = Nothing,
          agency = Nothing,
          fromArrivalTime = Nothing,
          fromDepartureTime = Nothing,
          toArrivalTime = Nothing,
          toDepartureTime = Nothing
        }

-- deleteLeg :: JourneyLeg leg m => leg -> m ()
-- deleteLeg leg = do
--   let cancelReq = mkCancelReq leg
--   JL.cancel cancelReq

-- updateLeg :: JourneyLeg leg m => leg -> leg -> m ()
-- updateLeg
--   let updateReq = mkUpdateReq leg
--   JL.update leg

-- skipJourney :: Journey -> [leg] -> m ()
-- skipJourney journey
-- getRemainingLegs
-- map update [leg]
-- @@ call cancel for current leg

-- endJourney :: Journey -> m ()
-- endJourney
-- if last leg then update leg
-- loop through and delete/update legs and journey as required
-- call leg level cancel

-- replaceLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m () -- leg2 can be an array
-- replaceLeg journey oldLegs newLeg =
--   forM_ (deleteLeg journey) oldLegs >> addLeg journey newLeg

-- extendLeg :: JourneyLeg leg1 leg2 m => Journey -> [leg1] -> leg2 -> m ()
-- extendLeg journey oldLegs newLeg =
--   forM_ (deleteLeg journey) oldLegs >> updateLeg journey newLeg
