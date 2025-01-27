module Lib.JourneyModule.Base where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.LocationAddress as LA
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Trip as DTrip
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
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
import qualified Storage.Queries.Journey as JQ
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

init ::
  JL.GetFareFlow m r =>
  JL.JourneyInitData ->
  m (Maybe DJourney.Journey)
init journeyReq = do
  journeyId <- Common.generateGUID
  mbTotalFares <-
    mapWithIndex
      ( \idx leg -> do
          let travelMode = convertMultiModalModeToTripMode leg.mode (distanceToMeters leg.distance) journeyReq.maximumWalkDistance
          mbTotalLegFare <- JLI.getFare journeyReq.merchantId journeyReq.merchantOperatingCityId leg travelMode
          whenJust mbTotalLegFare $ \fare -> do
            journeyLeg <- JL.mkJourneyLeg idx leg journeyReq.merchantId journeyReq.merchantOperatingCityId journeyId journeyReq.maximumWalkDistance fare
            QJourneyLeg.create journeyLeg
          return mbTotalLegFare
      )
      journeyReq.legs

  if any isNothing mbTotalFares
    then do return Nothing
    else do
      journey <- JL.mkJourney journeyReq.startTime journeyReq.endTime journeyReq.estimatedDistance journeyReq.estimatedDuration journeyId journeyReq.parentSearchId journeyReq.merchantId journeyReq.merchantOperatingCityId journeyReq.legs journeyReq.maximumWalkDistance
      QJourney.create journey
      logDebug $ "journey for multi-modal: " <> show journey
      return $ Just journey

getJourney :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m DJourney.Journey
getJourney id = JQ.findByPrimaryKey id >>= fromMaybeM (JourneyNotFound id.getId)

getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DJourney.Journey -> m [DJourneyLeg.JourneyLeg]
getJourneyLegs journeyId = do
  legs <- QJourneyLeg.findAllByJourneyId journeyId
  return $ sortBy (comparing (.sequenceNumber)) legs

getAllLegsInfo ::
  ( EsqDBFlow m r,
    Monad m,
    CacheFlow m r,
    EncFlow m r
  ) =>
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
            DTrip.Metro -> JL.getInfo $ MetroLegRequestGetInfo $ MetroLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare}
            DTrip.Subway -> JL.getInfo $ SubwayLegRequestGetInfo $ SubwayLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare}
            DTrip.Bus -> JL.getInfo $ BusLegRequestGetInfo $ BusLegRequestGetInfoData {searchId = cast legSearchId, fallbackFare = leg.estimatedMinFare}
        Nothing -> throwError $ InvalidRequest ("LegId null for Mode: " <> show leg.mode)
  return $ allLegsInfo

getAllLegsStatus ::
  ( EsqDBFlow m r,
    Monad m,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id DJourney.Journey ->
  m [JL.JourneyLegState]
getAllLegsStatus journeyId = do
  allLegsRawData <- getJourneyLegs journeyId
  riderLastPoints <- getLastThreePoints journeyId
  (_, allLegsState) <- foldlM (processLeg riderLastPoints) (True, []) allLegsRawData
  return allLegsState
  where
    processLeg ::
      ( EsqDBFlow m r,
        Monad m,
        CacheFlow m r,
        EncFlow m r
      ) =>
      [APITypes.RiderLocationReq] ->
      (Bool, [JL.JourneyLegState]) ->
      DJourneyLeg.JourneyLeg ->
      m (Bool, [JL.JourneyLegState])
    processLeg riderLastPoints (isLastJustCompleted, legsState) leg = do
      case leg.legSearchId of
        Just legSearchIdText -> do
          let legSearchId = Id legSearchIdText
          legState <-
            case leg.mode of
              DTrip.Taxi -> JL.getState $ TaxiLegRequestGetState $ TaxiLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastJustCompleted}
              DTrip.Walk -> JL.getState $ WalkLegRequestGetState $ WalkLegRequestGetStateData {walkLegId = cast legSearchId, riderLastPoints, isLastJustCompleted}
              DTrip.Metro -> JL.getState $ MetroLegRequestGetState $ MetroLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastJustCompleted}
              DTrip.Subway -> JL.getState $ SubwayLegRequestGetState $ SubwayLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastJustCompleted}
              DTrip.Bus -> JL.getState $ BusLegRequestGetState $ BusLegRequestGetStateData {searchId = cast legSearchId, riderLastPoints, isLastJustCompleted}
          return (legState.statusChanged && legState.status == JL.Completed, legsState <> [legState])
        Nothing -> throwError $ InvalidRequest ("LegId null for Mode: " <> show leg.mode)

startJourney ::
  JL.ConfirmFlow m r c =>
  Id DJourney.Journey ->
  m ()
startJourney journeyId = do
  allLegs <- getAllLegsInfo journeyId
  mapM_ JLI.confirm allLegs

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
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    MonadTime m
  ) =>
  Id DJourney.Journey ->
  m (Maybe JL.LegInfo)
getCurrentLeg journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let currentLeg = find (\leg -> leg.status `notElem` JL.completedStatus) journeyLegs
  return currentLeg

getRemainingLegs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Id DJourney.Journey ->
  m [JL.LegInfo]
getRemainingLegs journeyId = do
  journeyLegs <- getAllLegsInfo journeyId
  let remainingLegs = dropWhile (\leg -> leg.status `notElem` JL.completedStatus) journeyLegs -- check if edge case is to be handled [completed , skipped, inplan]
  return remainingLegs

getUnifiedQR :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [JL.LegInfo] -> m JL.UnifiedTicketQR
getUnifiedQR legs = do
  bookings <- mapM getTickets (filter (\leg -> leg.travelMode `elem` [DTrip.Metro, DTrip.Bus]) legs)
  let cmrlBookings = [b | (provider, b) <- bookings, provider == providerToText JL.CMRL]
  let mtcBookings = [b | (provider, b) <- bookings, provider == providerToText JL.MTC]
  now <- getCurrentTime
  return
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

getTickets :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => JL.LegInfo -> m (Text, JL.BookingData)
getTickets leg = do
  bookingId <- leg.pricingId & fromMaybeM (InvalidRequest $ "Booking pending for: " <> show leg.travelMode)
  case leg.legExtraInfo of
    JL.Metro info -> processTickets JL.CMRL info.tickets info.providerName bookingId
    JL.Bus info -> processTickets JL.MTC info.tickets info.providerName bookingId
    _ -> throwError (InvalidRequest $ "Tickets not generated yet for: " <> show leg.travelMode)
  where
    processTickets :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => JL.Provider -> Maybe [Text] -> Maybe Text -> Text -> m (Text, JL.BookingData)
    processTickets expectedProvider mbTickets mbProviderName bookingId = do
      tickets <- fromMaybeM (InvalidRequest "Tickets not found") mbTickets
      provider <- fromMaybeM (InvalidRequest "Provider not found") mbProviderName
      if provider == providerToText expectedProvider
        then
          return
            ( provider,
              JL.BookingData
                { bookingId = bookingId,
                  isRoundTrip = False,
                  ticketData = tickets
                }
            )
        else throwError (InvalidRequest $ "Unknown provider: " <> provider)

cancelLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  JL.LegInfo ->
  m ()
cancelLeg journeyLeg = do
  unless (journeyLeg.status `elem` [JL.Ongoing, JL.Finishing, JL.Cancelled, JL.Completed]) $
    throwError $ InvalidRequest $ "Leg cannot be skipped in status: " <> show journeyLeg.status
  case journeyLeg.travelMode of
    DTrip.Taxi -> JL.cancel $ TaxiLegRequestCancel TaxiLegRequestCancelData
    DTrip.Walk -> JL.cancel $ WalkLegRequestCancel WalkLegRequestCancelData
    DTrip.Metro -> JL.cancel $ MetroLegRequestCancel MetroLegRequestCancelData
    DTrip.Subway -> JL.cancel $ SubwayLegRequestCancel SubwayLegRequestCancelData
    DTrip.Bus -> JL.cancel $ BusLegRequestCancel BusLegRequestCancelData
  return ()

cancelRemainingLegs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Id DJourney.Journey ->
  m ()
cancelRemainingLegs journeyId = do
  remainingLegs <- getRemainingLegs journeyId
  results <- forM remainingLegs $ \leg -> try @_ @SomeException (cancelLeg leg)
  let failures = [e | Left e <- results]
  unless (null failures) $
    throwError $ InternalError $ "Failed to cancel some legs: " <> show failures
  return ()

skipLeg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Id DJourney.Journey ->
  Id DJourneyLeg.JourneyLeg ->
  m ()
skipLeg journeyId legId = do
  allLegs <- getAllLegsInfo journeyId
  skippingLeg <-
    find
      (\leg -> maybe False (\id -> Kernel.Types.Id.Id id == legId) leg.pricingId)
      allLegs
      & fromMaybeM (InvalidRequest $ "Leg not found: " <> legId.getId)

  unless (skippingLeg.status `elem` [JL.Ongoing, JL.Finishing, JL.Cancelled, JL.Completed]) $
    throwError $ InvalidRequest $ "Leg cannot be skipped in status: " <> show skippingLeg.status

  cancelLeg skippingLeg

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
