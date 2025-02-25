module Lib.JourneyModule.Base where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortBy)
import Data.Ord (comparing)
import Domain.Action.UI.EditLocation as DEditLocation
import qualified Domain.Action.UI.Location as DLoc
import Domain.Action.UI.Ride as DRide
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
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
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common as Common
import Kernel.Types.Distance
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Types.Price as KTP
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
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.Journey as JQ
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg
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

getJourneyFare :: [JL.LegInfo] -> Flow Price
getJourneyFare legs =
  case catMaybes (map (.totalFare) legs) of
    [] -> pure $ Price {amount = HighPrecMoney 0, amountInt = Money 0, currency = KTP.INR}
    (firstFare : restFares) -> foldlM KTP.addPrice firstFare restFares

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
  let lockKey = multimodalLegSearchIdAccessLockKey journeyId.getId
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
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

getAllLegsStatus ::
  JL.GetStateFlow m r c =>
  DJourney.Journey ->
  m [JL.JourneyLegState]
getAllLegsStatus journey = do
  allLegsRawData <- getJourneyLegs journey.id
  riderLastPoints <- getLastThreePoints journey.id
  let lockKey = multimodalLegSearchIdAccessLockKey journey.id.getId
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
    (_, allLegsState) <- foldlM (processLeg riderLastPoints) (True, []) allLegsRawData
    when (all (\st -> st.status `elem` [JL.Completed, JL.Skipped, JL.Cancelled]) allLegsState && journey.status /= DJourney.CANCELLED) $ do
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
  mapM_
    ( \leg ->
        when (leg.status /= JL.Skipped) $
          JLI.confirm (if (leg.order == 0) then True else (Just leg.order == forcedBookedLegOrder)) leg
    )
    allLegs

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
  let cmrlBookings = [b | (provider, b) <- bookings, provider == providerToText JL.CMRL || provider == providerToText JL.DIRECT]
  let mtcBookings = [b | (provider, b) <- bookings, provider == providerToText JL.MTC || provider == providerToText JL.DIRECT]
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
providerToText JL.DIRECT = "Direct Multimodal Services"

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
      if provider == providerToText expectedProvider || provider == providerToText JL.DIRECT
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
  -- forM_ remainingLegs $ \leg -> do
  --   isCancellable <- checkIfCancellable leg
  --   unless isCancellable $
  --     throwError $ InvalidRequest $ "Cannot cancel leg for leg: " <> show leg.travelMode
  results <-
    forM remainingLegs $ \leg -> do
      try @_ @SomeException $
        if leg.skipBooking
          then return ()
          else do
            isCancellable <- checkIfCancellable leg
            if isCancellable then (cancelLeg leg (SCR.CancellationReasonCode "")) False else (QJourneyLeg.updateIsDeleted (Just True) (Just leg.searchId))
  let failures = [e | Left e <- results]
  unless (null failures) $
    throwError $ InvalidRequest $ "Failed to cancel some legs: " <> show failures

multimodalLegSearchIdAccessLockKey :: Text -> Text
multimodalLegSearchIdAccessLockKey legSearchId = "Multimodal:Leg:SearchIdAccess:" <> legSearchId

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

addSkippedLeg ::
  (JL.GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) =>
  Id DJourney.Journey ->
  Int ->
  m ()
addSkippedLeg journeyId legOrder = do
  allLegs <- QJourneyLeg.findAllByJourneyId journeyId
  -- skippedLeg <- find (\leg -> (leg.isSkipped == Just True && leg.sequenceNumber == legOrder)
  --  || (leg.isDeleted == Just True && leg.sequenceNumber == legOrder && leg.mode == DTrip.Walk))
  --  allLegs & fromMaybeM (InvalidRequest $ "Skipped Leg not found with leg Order: " <> show allLegs)

  skippedLeg <-
    find
      (\leg -> (leg.sequenceNumber == legOrder))
      allLegs
      & fromMaybeM (InvalidRequest $ "Skipped Leg not found with leg Order: " <> show legOrder)

  legSearchId <-
    skippedLeg.legSearchId
      & fromMaybeM (InvalidRequest "legSearchId is missing for skippedLeg")

  isSkippedOrCancelled <- case skippedLeg.mode of
    DTrip.Taxi -> do
      mbBooking <- QBooking.findByTransactionId legSearchId
      case mbBooking of
        Just booking -> return booking.isSkipped
        Nothing -> do
          searchReq <- QSearchRequest.findById (Id legSearchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legSearchId)
          case searchReq.journeyLegInfo of
            Just journeyData -> return (Just journeyData.skipBooking)
            Nothing -> return Nothing
    DTrip.Walk -> do
      walkLeg <- QWalkLeg.findById (Id legSearchId) >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
      case walkLeg.journeyLegInfo of
        Just walkLegData -> return walkLegData.isDeleted
        Nothing -> return Nothing
    DTrip.Metro -> checkFRFSBooking legSearchId
    DTrip.Subway -> checkFRFSBooking legSearchId
    DTrip.Bus -> checkFRFSBooking legSearchId

  when (isSkippedOrCancelled /= Just True) $
    throwError $ InvalidRequest $ "isSkipped is not True for legOrder: " <> show legOrder
  when (skippedLeg.mode == DTrip.Walk) $
    QJourneyLeg.updateIsDeleted (Just False) skippedLeg.legSearchId

  let lockKey = multimodalLegSearchIdAccessLockKey journeyId.getId
  exep <- try @_ @SomeException $ do
    Redis.withLockRedis lockKey 5 $ do
      QJourneyLeg.updateIsSkipped (Just False) skippedLeg.legSearchId
      QJourneyLeg.updateLegSearchId Nothing skippedLeg.id
      addAllLegs journeyId
  case exep of
    Left _ -> do
      -- Rollback operations
      QJourneyLeg.updateLegSearchId skippedLeg.legSearchId skippedLeg.id
      QJourneyLeg.updateIsSkipped (Just True) skippedLeg.legSearchId
      when (skippedLeg.mode == DTrip.Walk) $
        QJourneyLeg.updateIsDeleted (Just True) skippedLeg.legSearchId
      throwError $ InvalidRequest "Failed to update skipped leg, as Search operation failed"
    Right _ -> return ()
  where
    checkFRFSBooking legSearchId = do
      frfsBooking <- QTBooking.findBySearchId (Id legSearchId)
      case frfsBooking of
        Just booking -> return booking.isSkipped
        Nothing -> do
          frfsSearchReq <- QFRFSSearch.findById (Id legSearchId) >>= fromMaybeM (SearchRequestNotFound $ "searchRequestId-" <> legSearchId)
          case frfsSearchReq.journeyLegInfo of
            Just frfsJourneyData -> return (Just frfsJourneyData.skipBooking)
            Nothing -> return Nothing

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
        routeDetails = [],
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
  Maybe (Id DBUR.BookingUpdateRequest) ->
  m ()
extendLeg journeyId startPoint mbEndLocation mbEndLegOrder fare newDistance newDuration bookingUpdateReqId = do
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
      -- checkIfRemainingLegsAreCancellable legsToCancel
      forM_ legsToCancel $ \leg -> do
        isCancellable <- checkIfCancellable leg
        if isCancellable
          then cancelLeg leg (SCR.CancellationReasonCode "") False
          else QJourneyLeg.updateIsDeleted (Just True) (Just leg.searchId)

      (newOriginLat, newOriginLon) <- getNewOriginLatLon currentLeg.legExtraInfo
      let leg = mkMultiModalLeg newDistance newDuration MultiModalTypes.Unspecified newOriginLat newOriginLon endLocation.lat endLocation.lon currentLeg.startTime
      riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
      journeyLeg <- JL.mkJourneyLeg startLegOrder leg currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance (Just fare)
      QJourneyLeg.create journeyLeg
      addAllLegs journeyId
    JL.StartLocation startlocation -> do
      currentLeg <- getCurrentLeg journeyId >>= fromMaybeM (InternalError "Current leg not found")
      case currentLeg.travelMode of
        DTrip.Taxi -> do
          bookingUpdateRequestId <- bookingUpdateReqId & fromMaybeM (InvalidRequest "bookingUpdateReqId not found")
          journeyLeg <- QJourneyLeg.findByLegSearchId (Just currentLeg.searchId) >>= fromMaybeM (InvalidRequest $ "JourneyLeg not found for searchId: " <> currentLeg.searchId)
          void $ DEditLocation.postEditResultConfirm (Just parentSearchReq.riderId, parentSearchReq.merchantId) bookingUpdateRequestId
          Redis.setExp mkExtendLegKey journeyLeg.id 300 --5 mins
        DTrip.Walk -> do
          cancelRequiredLegs
          now <- getCurrentTime
          let leg = mkMultiModalLeg newDistance newDuration MultiModalTypes.Unspecified startlocation.lat startlocation.lon endLocation.lat endLocation.lon now
          riderConfig <- QRC.findByMerchantOperatingCityId currentLeg.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist currentLeg.merchantOperatingCityId.getId)
          journeyLeg <- JL.mkJourneyLeg currentLeg.order leg currentLeg.merchantId currentLeg.merchantOperatingCityId journeyId riderConfig.maximumWalkDistance (Just fare)
          QJourneyLeg.create journeyLeg
          addAllLegs journeyId
          startJourney (Just currentLeg.order) journeyId
        _ -> do
          throwError $ InvalidRequest ("Cannot extend leg for mode: " <> show currentLeg.travelMode)
  where
    cancelRequiredLegs = do
      case mbEndLegOrder of
        Nothing -> cancelRemainingLegs journeyId
        Just endLegOrder -> do
          remainingLegs <- getRemainingLegs journeyId
          let legsToCancel = filter (\leg -> leg.order < endLegOrder) remainingLegs
          -- checkIfRemainingLegsAreCancellable legsToCancel
          mapM_
            ( \leg -> do
                isCancellable <- checkIfCancellable leg
                if isCancellable
                  then cancelLeg leg (SCR.CancellationReasonCode "") False
                  else QJourneyLeg.updateIsDeleted (Just True) (Just leg.searchId)
            )
            legsToCancel

    getNewOriginLatLon legExtraInfo =
      case legExtraInfo of
        JL.Walk info -> return (info.origin.lat, info.origin.lon)
        JL.Taxi info -> return (info.origin.lat, info.origin.lon)
        JL.Metro info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> getLatLon "Metro" firstRoute.originStop.lat firstRoute.originStop.lon
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Metro leg info"
        JL.Subway info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> getLatLon "Subway" firstRoute.originStop.lat firstRoute.originStop.lon
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Subway leg info"
        JL.Bus info -> getLatLon "Bus" info.originStop.lat info.originStop.lon

    getLatLon label mLat mLon = do
      lat <- fromMaybeM (InvalidRequest $ label <> " latitude not found") mLat
      lon <- fromMaybeM (InvalidRequest $ label <> " longitude not found") mLon
      return (lat, lon)

    mkMultiModalLeg distance duration mode originLat originLon destLat destLon startTime =
      MultiModalTypes.MultiModalLeg
        { distance,
          duration,
          polyline = Polyline {encodedPolyline = ""},
          mode,
          startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
          fromStopDetails = Nothing,
          toStopDetails = Nothing,
          routeDetails = [],
          agency = Nothing,
          fromArrivalTime = Just startTime,
          fromDepartureTime = Just startTime,
          toArrivalTime = Nothing,
          toDepartureTime = Nothing
        }

    mkExtendLegKey = "Extend:Leg:For:JourneyId-" <> journeyId.getId

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

  -- isLegsCancellable <- checkIfAllLegsCancellable remainingLegs
  -- if isLegsCancellable
  --   then do
  parentSearchReq <- QSearchRequest.findById journey.searchRequestId >>= fromMaybeM (SearchRequestNotFound journey.searchRequestId.getId)
  endLocation <- maybe (fromMaybeM (InvalidRequest $ "toLocation not found for searchId: " <> show parentSearchReq.id.getId) parentSearchReq.toLocation >>= return . DLoc.makeLocationAPIEntity) return mbEndLocation

  startLocation <- getStartLocation startPoint remainingLegs
  case (startPoint, currentLeg.travelMode) of
    (JL.StartLocation _, DTrip.Taxi) -> do
      booking <- QBooking.findByTransactionId currentLeg.searchId >>= fromMaybeM (BookingNotFound $ "transactionId:-" <> currentLeg.searchId)
      ride <- QRide.findByRBId booking.id >>= fromMaybeM (InvalidRequest $ "No Ride present for booking" <> booking.id.getId)
      let editLocReq =
            DRide.EditLocationReq
              { origin = Nothing,
                destination = Just $ DRide.EditLocation {gps = LatLong {lat = endLocation.lat, lon = endLocation.lon}, address = getAddress endLocation}
              }
      editLocResp <- DRide.editLocation ride.id (currentLeg.personId, currentLeg.merchantId) editLocReq -- handle case if driver declines
      case editLocResp.bookingUpdateRequestId of
        Just bookingUpdateReqId -> do
          searchForUpdateRequest bookingUpdateReqId (5 :: Int) -- can set in config
        Nothing -> throwError (InvalidRequest "bookingUpdateRequestId not found")
    (_, _) -> do
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
            duration = Just distResp.duration,
            bookingUpdateRequestId = Nothing
          }
  where
    -- else throwError (InvalidRequest "Legs are not cancellable")

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
        JL.Metro info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> mkLatLngFromFRFS firstRoute.originStop -- Fetch the stop where subOrder is 1
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Metro info"
        JL.Bus info -> mkLatLngFromFRFS info.originStop
        JL.Subway info ->
          case find (\route -> route.subOrder == Just 1) info.routeInfo of
            Just firstRoute -> mkLatLngFromFRFS firstRoute.originStop -- Fetch the stop where subOrder is 1
            Nothing -> throwM $ InvalidRequest "No route with subOrder 1 found in Subway info"

    mkLatLng originLocation = pure $ LatLong {lat = originLocation.lat, lon = originLocation.lon}
    mkLatLngFromFRFS startLoc = do
      latitude <- startLoc.lat & fromMaybeM (InvalidRequest "Start location not Found")
      longitude <- startLoc.lon & fromMaybeM (InvalidRequest "Start location not Found")
      return $ LatLong {lat = latitude, lon = longitude}

    searchForUpdateRequest bookingUpdateReqId 0 = throwError (InvalidRequest $ "Maximum number of tries reached for editLocation results for bookingUpdateReqId: " <> show bookingUpdateReqId)
    searchForUpdateRequest bookingUpdateReqId count = do
      bookingUpdateReq <- QBUR.findById bookingUpdateReqId >>= fromMaybeM (InvalidRequest $ "bookingUpdateRequest not found for id: " <> show bookingUpdateReqId)
      case bookingUpdateReq.estimatedFare of
        Nothing -> do
          liftIO $ threadDelaySec $ Seconds {getSeconds = 2} -- can set in config
          searchForUpdateRequest bookingUpdateReqId (count -1)
        Just estimatedFare -> do
          estimatedDistance <- bookingUpdateReq.estimatedDistance & fromMaybeM (InvalidRequest $ "EditLocation distance not Found for bookingUpdateReqId: " <> show bookingUpdateReq.id)
          return $
            APITypes.ExtendLegGetFareResp
              { totalFare = Just JL.GetFareResponse {estimatedMinFare = estimatedFare, estimatedMaxFare = estimatedFare},
                distance = convertHighPrecMetersToDistance bookingUpdateReq.distanceUnit estimatedDistance,
                duration = Nothing,
                bookingUpdateRequestId = Just bookingUpdateReq.id
              }

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
          routeDetails = [],
          agency = Nothing,
          fromArrivalTime = Nothing,
          fromDepartureTime = Nothing,
          toArrivalTime = Nothing,
          toDepartureTime = Nothing
        }

    getAddress DLocation.LocationAPIEntity {..} = LA.LocationAddress {..}

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
