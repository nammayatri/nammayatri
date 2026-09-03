module SharedLogic.FRFSConfirm where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra hiding (fromMaybeM)
import qualified Data.Hashable as Hashable
import Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty hiding (groupBy, map, nub, nubBy)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import qualified Domain.Types.PurchasedPassPayment as DPPP
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.RouteDetails as DRD
import qualified Domain.Types.Seat as Seat
import qualified Domain.Types.Trip as DTrip
import qualified Domain.Types.VehicleSeatLayoutMapping as DVSLM
import EulerHS.Prelude hiding (all, and, any, concatMap, elem, find, foldr, forM_, fromList, groupBy, hoistMaybe, id, length, map, mapM_, maximum, minimumBy, null, readMaybe, toList, whenJust)
import qualified ExternalBPP.CallAPI.Confirm as CallExternalBPP
import qualified ExternalBPP.CallAPI.Init as CallExternalBPP
import qualified ExternalBPP.CallAPI.Types as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Interface.Types
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.MultiModal.Interface.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (whenJust)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common hiding (mkPrice)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.FRFSReschedule as FRFSReschedule
import qualified SharedLogic.FRFSSeatBooking as SeatBooking
import SharedLogic.FRFSStatus
import SharedLogic.FRFSUtils
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Seat as QSeat
import qualified Storage.CachedQueries.VehicleSeatLayoutMappingExtra as CQVehicleSeatLayoutMapping
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Location as QLocation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RouteDetails as QRouteDetails
import Tools.Error
import Tools.Maps as Maps
import Tools.Metrics.BAPMetrics (HasBAPMetrics)

data SeatHoldParams = SeatHoldParams
  { shpFromIdx :: Int,
    shpToIdx :: Int,
    shpDefaultTtl :: Int,
    shpSeatBitMapTtl :: Int
  }

-- | Set when the created booking is a reschedule staging booking: it becomes the old booking's child
-- (parentBookingId) and is pinned to the old payment (oldFrfsPaymentId) so no second payment row is made.
data RescheduleCtx = RescheduleCtx
  { oldBookingId :: Id DFRFSTicketBooking.FRFSTicketBooking,
    oldFrfsPaymentId :: Maybe (Id DFRFSTicketBookingPayment.FRFSTicketBookingPayment)
  }

confirmAndUpsertBooking :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "cloudType" r (Maybe CloudType)) => Id Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq] -> Maybe CrisSdkResponse -> Maybe Bool -> Maybe Bool -> DIBC.IntegratedBPPConfig -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe RescheduleCtx -> Maybe (Id DPPP.PurchasedPassPayment) -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking, FRFSUtils.FRFSFareParameters, [FRFSQuoteCategory.FRFSQuoteCategory], Bool)
confirmAndUpsertBooking personId quote selectedQuoteCategories crisSdkResponse isSingleMode mbIsMockPayment integratedBppConfig mbTripId isSpotBooking mbVehicleNumber mbRescheduleCtx mbPurchasedPassPaymentId = do
  Hedis.withWaitAndLockMasterCloudCrossAppRedis (mkConfirmLockKey quote.searchId.getId) confirmLockTtlSec confirmLockRetryDelayMicros $ do
    quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
    mbBooking <- QFRFSTicketBooking.findBySearchId quote.searchId
    riderConfig <-
      getConfig (RiderConfigDimensions {merchantOperatingCityId = integratedBppConfig.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM
          (RiderConfigNotFound $ "merchantOpCityid: " <> integratedBppConfig.merchantOperatingCityId.getId)
    isMultiInitAllowed <-
      case mbBooking of
        Just booking -> do
          case integratedBppConfig.providerConfig of
            DIBC.ONDC DIBC.ONDCBecknConfig {multiInitAllowed} ->
              return $
                multiInitAllowed == Just True
                  && booking.status `elem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
            _ -> return $ booking.status `elem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
        Nothing -> return True
    let mbConfirmVehicleNumber = quote.vehicleNumber <|> mbVehicleNumber
    mbSeatLayoutMapping <- case mbConfirmVehicleNumber of
      Just vNo -> CQVehicleSeatLayoutMapping.findByVehicleNoAndGtfsIdCached vNo integratedBppConfig.feedKey
      Nothing -> pure Nothing
    let seatSelectionType = mbSeatLayoutMapping >>= (.seatSelectionType)
        shouldAutoAssignBusSeats = quote.vehicleType == Spec.BUS && isJust mbTripId && seatSelectionType == Just DVSLM.AUTO_ASSIGNED
    (selectedQuoteCategoriesFinal, mbHoldCtxForAll) <-
      if shouldAutoAssignBusSeats
        then do
          tripId <- mbTripId & fromMaybeM (InvalidRequest "TripId not found for bus auto-seat flow")
          let requiredSeatCount = Kernel.Prelude.sum ((.quantity) <$> selectedQuoteCategories)
          if requiredSeatCount <= 0
            then pure (clearSeatIds selectedQuoteCategories, Nothing)
            else do
              logInfo $ "FRFSConfirm:confirmAndUpsertBooking bus auto-seat flow personId=" <> personId.getId <> " tripId=" <> tripId <> " requiredSeatCount=" <> show requiredSeatCount
              mbSeatHoldParams <- getSeatHoldParams tripId riderConfig
              case mbSeatHoldParams of
                Nothing -> pure (clearSeatIds selectedQuoteCategories, Nothing)
                Just params -> do
                  let maxAttempts = 3
                  orderedSeatIds <- getOrderedSeatIds tripId params.shpFromIdx params.shpToIdx mbSeatLayoutMapping
                  (chosenSeatIds, holdId) <- selectAndHoldWithRetries tripId orderedSeatIds params.shpFromIdx params.shpToIdx params.shpDefaultTtl params.shpSeatBitMapTtl requiredSeatCount maxAttempts
                  let selectedQuoteCategories' = assignSeatsToCategories chosenSeatIds selectedQuoteCategories
                  pure (selectedQuoteCategories', Just (holdId, params.shpFromIdx, params.shpToIdx))
        else do
          let allSeatIds = nub $ concatMap (\categoryReq -> fromMaybe [] categoryReq.seatIds) selectedQuoteCategories
          mbHoldCtx <-
            case (mbTripId, allSeatIds) of
              (Just tripId, _ : _) -> do
                logInfo $
                  "FRFSConfirm:confirmAndUpsertBooking seatHold flow personId=" <> personId.getId <> " tripId=" <> tripId <> " seatCount=" <> show (length allSeatIds)
                mbSeatHoldParams <- getSeatHoldParams tripId riderConfig
                case mbSeatHoldParams of
                  Nothing -> pure Nothing
                  Just params -> do
                    holdId <- generateGUID
                    seats <- mapM QSeat.findById allSeatIds
                    case mapM_ (validateQuota params.shpFromIdx params.shpToIdx) seats of
                      Left err -> throwError err
                      Right () -> pure ()
                    success <- SeatBooking.holdSeats tripId allSeatIds params.shpFromIdx params.shpToIdx holdId params.shpDefaultTtl params.shpSeatBitMapTtl
                    unless success $ throwError (SeatsNotFound (map (.getId) allSeatIds))
                    pure $ Just (holdId, params.shpFromIdx, params.shpToIdx)
              _ -> pure Nothing
          pure (selectedQuoteCategories, mbHoldCtx)
    quoteCategorySelections <-
      if isMultiInitAllowed
        then mapM processCategorySelection selectedQuoteCategoriesFinal
        else return $ quoteCategories <&> (\qc -> FRFSUtils.QuoteCategorySelection qc.id qc.selectedQuantity Nothing Nothing)
    let mbHoldId = mbHoldCtxForAll <&> (\(h, _, _) -> h)
    updatedQuoteCategories <-
      if isMultiInitAllowed
        then FRFSUtils.updateQuoteCategoriesWithSelections mbHoldId quoteCategorySelections quoteCategories
        else return quoteCategories
    let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories updatedQuoteCategories)
    -- The hold is taken above but only tracked against a booking below, so a throw in between leaves
    -- those seats held until their TTL, and repeated attempts block the trip.
    --
    -- Scoped to requests that supplied a pass, because that is the throw this PR adds:
    -- resolvePassOverride rejects an inapplicable pass with InvalidRequest, after the hold exists.
    -- A non-pass booking keeps main's behaviour exactly -- the hold still leaks on an unrelated
    -- throw, which is pre-existing and filed separately rather than fixed inside a pass PR.
    confirmResult <- try @_ @SomeException $ confirm isMultiInitAllowed fareParameters mbBooking mbHoldCtxForAll mbTripId seatSelectionType isSpotBooking
    (rider, dConfirmRes) <- case confirmResult of
      Right res -> pure res
      Left err -> do
        when (isJust mbPurchasedPassPaymentId) $
          whenJust ((,) <$> mbTripId <*> mbHoldId) $ \(tripId, holdId) -> do
            logWarning $ "FRFSConfirm:confirmAndUpsertBooking releasing hold after a pass failure holdId=" <> holdId <> " tripId=" <> tripId <> " err=" <> show err
            void $ withTryCatch "FRFSConfirm:releaseHoldOnFailure" (SeatBooking.releaseHold tripId holdId)
        throwM err
    whenJust mbHoldCtxForAll $ \(holdId, _, _) -> do
      logInfo $ "FRFSConfirm:confirmAndUpsertBooking tracking hold bookingId=" <> dConfirmRes.id.getId <> " holdId=" <> holdId
      SeatBooking.trackHoldForBooking dConfirmRes.id.getId holdId (fromMaybe 600 riderConfig.seatBookingTtl)
    return (rider, dConfirmRes, fareParameters, updatedQuoteCategories, isMultiInitAllowed)
  where
    confirmLockTtlSec :: Int
    confirmLockTtlSec = 60

    confirmLockRetryDelayMicros :: Int
    confirmLockRetryDelayMicros = 100

    mkConfirmLockKey :: Text -> Text
    mkConfirmLockKey searchId = "frfs:confirm:searchId-" <> searchId

    clearSeatIds :: [FRFSCategorySelectionReq] -> [FRFSCategorySelectionReq]
    clearSeatIds = map (\FRFSCategorySelectionReq {quantity, quoteCategoryId} -> FRFSCategorySelectionReq {quantity, quoteCategoryId, seatIds = Nothing})

    getSeatHoldParams :: (CallExternalBPP.FRFSConfirmFlow m r c) => Text -> DRC.RiderConfig -> m (Maybe SeatHoldParams)
    getSeatHoldParams tripId riderCfg = do
      let routeStations :: Maybe [FRFSTicketService.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
      let mbRouteCode = listToMaybe (fromMaybe [] routeStations) <&> (.code)
      case mbRouteCode of
        Nothing -> do
          logWarning $ "FRFSConfirm:confirmAndUpsertBooking skipping hold, routeCode not found for quoteId=" <> quote.id.getId
          pure Nothing
        Just routeCode -> do
          mIndices <- JourneyUtils.getRouteStopIndices routeCode quote.fromStationCode quote.toStationCode integratedBppConfig
          case mIndices of
            Nothing -> do
              logWarning $ "FRFSConfirm:confirmAndUpsertBooking skipping hold, stop indices not found for routeCode=" <> routeCode <> " from=" <> quote.fromStationCode <> " to=" <> quote.toStationCode
              pure Nothing
            Just (fromIdx, toIdx) -> do
              let defaultTtl = fromMaybe 600 riderCfg.seatBookingTtl
                  bufferTime = fromMaybe 172800 riderCfg.busTripTtl
              seatBitMapTtl <- calculateDynamicSeatHoldTTL tripId routeCode integratedBppConfig bufferTime
              pure $ Just (SeatHoldParams fromIdx toIdx defaultTtl seatBitMapTtl)

    assignSeatsToCategories :: [Id Seat.Seat] -> [FRFSCategorySelectionReq] -> [FRFSCategorySelectionReq]
    assignSeatsToCategories seatIds categories =
      let step :: [Id Seat.Seat] -> FRFSCategorySelectionReq -> ([Id Seat.Seat], FRFSCategorySelectionReq)
          step remaining FRFSCategorySelectionReq {quantity, quoteCategoryId} =
            let q = max 0 quantity
                (taken, rest) = splitAt q remaining
                updatedSeatIds =
                  if q <= 0
                    then Nothing
                    else Just taken
             in (rest, FRFSCategorySelectionReq {quantity, quoteCategoryId, seatIds = updatedSeatIds})
       in snd (mapAccumL step seatIds categories)

    selectAndHoldWithRetries :: (CallExternalBPP.FRFSConfirmFlow m r c) => Text -> [Id Seat.Seat] -> Int -> Int -> Int -> Int -> Int -> Int -> m ([Id Seat.Seat], Text)
    selectAndHoldWithRetries tripId orderedSeatIds fromIdx toIdx defaultTtl seatBitMapTtl requiredSeatCount maxAttempts =
      go 0
      where
        go attempt = do
          let offset = attempt * requiredSeatCount
              chosen = take requiredSeatCount (drop offset orderedSeatIds)
          when (length chosen < requiredSeatCount) $
            throwError (InvalidRequest "Not enough seats available.")
          holdId <- generateGUID
          logInfo $ "FRFSConfirm:confirmAndUpsertBooking auto-seat attempt=" <> show (attempt + 1) <> "/" <> show maxAttempts <> " tripId=" <> tripId <> " seatCount=" <> show requiredSeatCount
          success <- SeatBooking.holdSeats tripId chosen fromIdx toIdx holdId defaultTtl seatBitMapTtl
          if success
            then do
              logInfo $ "FRFSConfirm:confirmAndUpsertBooking auto-seat SUCCESS tripId=" <> tripId <> " holdId=" <> holdId <> " seatIds=" <> show (map (.getId) chosen)
              pure (chosen, holdId)
            else
              if attempt + 1 >= maxAttempts
                then do
                  logWarning $ "FRFSConfirm:confirmAndUpsertBooking auto-seat FAILED tripId=" <> tripId <> " attempts=" <> show maxAttempts
                  throwError (SeatsNotFound (map (.getId) chosen))
                else go (attempt + 1)

    processCategorySelection :: CallExternalBPP.FRFSConfirmFlow m r c => FRFSCategorySelectionReq -> m FRFSUtils.QuoteCategorySelection
    processCategorySelection categoryReq = do
      mbLabels <- case categoryReq.seatIds of
        Just categorySeatIds | not (null categorySeatIds) -> do
          seats <- mapM QSeat.findById categorySeatIds
          let sLabels = map (.seatLabel) (catMaybes seats)
          pure $ if null sLabels then Nothing else Just sLabels
        _ -> pure Nothing

      return $
        FRFSUtils.QuoteCategorySelection
          { qcQuoteCategoryId = categoryReq.quoteCategoryId,
            qcQuantity = categoryReq.quantity,
            qcSeatIds = categoryReq.seatIds,
            qcSeatLabels = mbLabels
          }

    getOrderedSeatIds ::
      (CallExternalBPP.FRFSConfirmFlow m r c) =>
      Text -> -- tripId
      Int -> -- fromIdx
      Int -> -- toIdx
      Maybe DVSLM.VehicleSeatLayoutMapping ->
      m [Id Seat.Seat]
    getOrderedSeatIds tripId fromIdx toIdx mbSeatLayoutMapping' = do
      seatLayoutMapping <- mbSeatLayoutMapping' & fromMaybeM (InvalidRequest "Seat layout mapping not found for vehicle")
      let seatLayoutId = seatLayoutMapping.seatLayoutId
      seats <- QSeat.findAllByLayoutId seatLayoutId
      rawSeatsWithStatus <- SeatBooking.getTripAvailability tripId fromIdx toIdx seats
      seed <- generateGUID
      let candidates =
            filter
              ( \s ->
                  s.status == FRFSTicketService.AVAILABLE
                    && JourneyUtils.meetsSeatQuota fromIdx toIdx s.seat
              )
              rawSeatsWithStatus
      let orderedCandidates =
            sortOn
              (\s -> Hashable.hash (seed <> s.seat.id.getId))
              candidates
      let orderedSeatIds = map (\s -> s.seat.id) orderedCandidates
      pure orderedSeatIds

    confirm :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "cloudType" r (Maybe CloudType)) => Bool -> FRFSUtils.FRFSFareParameters -> Maybe DFRFSTicketBooking.FRFSTicketBooking -> Maybe (Text, Int, Int) -> Maybe Text -> Maybe DVSLM.SeatSelectionType -> Maybe Bool -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    confirm isMultiInitAllowed fareParameters mbBooking mbHoldCtxForAll firstTripId mbSeatSelectionType isSpotBooking' = do
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ FRFSQuoteExpired quote.id.getId
      unless (personId == quote.riderId) $ throwError AccessDenied
      maybeM
        (buildAndCreateBooking rider quote fareParameters mbIsMockPayment mbHoldCtxForAll firstTripId mbSeatSelectionType isSpotBooking' mbPurchasedPassPaymentId)
        ( \booking -> do
            updatedBooking <-
              if isMultiInitAllowed
                then do
                  let mBookAuthCode = crisSdkResponse <&> (.bookAuthCode)
                      totalPrice = fareParameters.totalPrice
                      mbNewServiceTierType = FRFSUtils.getServiceTierTypeFromRouteStationsJson quote.routeStationsJson
                  void $ QFRFSTicketBooking.updateBookingAuthCodeById mBookAuthCode booking.id
                  void $ QFRFSTicketBooking.updateQuoteBppItemIdRouteStationsAndServiceTierById quote.id quote.bppItemId quote.routeStationsJson mbNewServiceTierType booking.id
                  void $ QFRFSTicketBooking.updateIsFareChangedById Nothing booking.id
                  return $ booking {DFRFSTicketBooking.quoteId = quote.id, DFRFSTicketBooking.bppItemId = quote.bppItemId, DFRFSTicketBooking.bookingAuthCode = mBookAuthCode, DFRFSTicketBooking.totalPrice = totalPrice, DFRFSTicketBooking.serviceTierType = mbNewServiceTierType}
                else return booking
            pure (rider, updatedBooking)
        )
        (pure mbBooking)

    validateQuota :: Int -> Int -> Maybe Seat.Seat -> Either GenericError ()
    validateQuota fromIdx toIdx mbSeat =
      case mbSeat of
        Nothing ->
          Left (InvalidRequest "Selected seat not found.")
        Just seat ->
          if JourneyUtils.meetsSeatQuota fromIdx toIdx seat
            then Right ()
            else Left (InvalidRequest "One or more selected seats are reserved for longer journeys.")

    buildAndCreateBooking :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "cloudType" r (Maybe CloudType)) => Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> FRFSUtils.FRFSFareParameters -> Maybe Bool -> Maybe (Text, Int, Int) -> Maybe Text -> Maybe DVSLM.SeatSelectionType -> Maybe Bool -> Maybe (Id DPPP.PurchasedPassPayment) -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    buildAndCreateBooking rider quote'@DFRFSQuote.FRFSQuote {..} fareParameters mbMockPayment mbHoldCtxForAll firstTripId mbSeatSelectionType isSpotBooking' mbPurchasedPassPaymentId' = do
      uuid <- generateGUID
      now <- getCurrentTime
      mbSearch <- QFRFSSearch.findById searchId
      cloudType <- asks (.cloudType)
      let isFareChanged = if isJust partnerOrgId then isJust oldCacheDump else False
      let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< routeStationsJson
      let mbFirstRouteStation = listToMaybe (fromMaybe [] routeStations)
      let mbRouteCode = mbFirstRouteStation <&> (.code)
      let mbRouteName = mbFirstRouteStation <&> (.longName)
      let mbServiceTierType = mbFirstRouteStation >>= (.vehicleServiceTier) <&> (._type)

      -- Derive the real scheduled departure time for the trip so time-tiered logic
      -- (e.g. cancellation charges in ExternalBPP/Flow/Common.hs) is measured against the
      -- actual bus departure rather than the booking creation time. Bus-only: metro/subway
      -- have no waybill schedule and leave firstTripId Nothing, so they fall back to `now`.
      mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just searchId.getId)
      -- Only for a pass booking, where the departure decides which day the pass window is checked
      -- against. Non-pass bookings keep the original `now`. Also rejects a departure that is not in
      -- the future: journey_leg.fromDepartureTime comes from a timetable lookup, and where there is
      -- no timing for the stop it is the epoch (95% of subway legs), which would otherwise land in
      -- booking.startTime and feed calculateCancellationCharges.
      let mbLegDepartureTime =
            if isJust mbPurchasedPassPaymentId'
              then mfilter (> now) (mbJourneyLeg >>= (.fromDepartureTime))
              else Nothing
      bookingStartTime <-
        case (firstTripId, mbRouteCode) of
          (Just tripId, Just routeCode) -> do
            mbScheduledStartTime <- getScheduledTripStartTime tripId routeCode quote'.fromStationCode integratedBppConfig
            case mbScheduledStartTime of
              Just scheduledStartTime -> pure scheduledStartTime
              Nothing -> do
                logWarning $ "buildAndCreateBooking: no scheduled departure resolved for tripId=" <> tripId <> ", falling back to leg departure or booking time for startTime"
                pure (fromMaybe now mbLegDepartureTime)
          _ -> pure (fromMaybe now mbLegDepartureTime)

      -- No fallback to totalPrice: that is the quantity-multiplied order total, and handing it to
      -- resolvePassOverride as a UNIT price mis-values the override. The offer side
      -- (Lib.JourneyModule.Types) offers no pass at all when there is no ADULT item, so treating it
      -- as "no override applicable" here is what keeps the two sides agreeing.
      let mbAdultUnitPriceForOverride =
            find (\priceItem -> priceItem.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice)
      mbResolved <-
        case (mbPurchasedPassPaymentId', mbAdultUnitPriceForOverride) of
          (Just paymentId, Nothing) -> do
            -- Rejected rather than silently ignored, matching the "not applicable" throw below:
            -- honouring the request without the override would charge the rider the full fare
            -- having been told a pass applied.
            logWarning $ "FRFSConfirm: pass selected but no ADULT price item to price the override against purchasedPassPaymentId=" <> paymentId.getId
            throwError (InvalidRequest $ "Selected pass is not applicable to this booking, purchasedPassPaymentId=" <> paymentId.getId)
          (Nothing, _) -> pure Nothing
          (Just paymentId, Just adultUnitPriceForOverride) -> do
            resolved <-
              FRFSPassOverride.resolvePassOverride
                integratedBppConfig
                rider
                quote'.vehicleType
                bookingStartTime
                mbServiceTierType
                adultUnitPriceForOverride
                (map (\priceItem -> (priceItem.unitPrice, priceItem.quantity)) fareParameters.priceItems)
                paymentId
            when (isNothing resolved) $
              throwError (InvalidRequest $ "Selected pass is not applicable to this booking, purchasedPassPaymentId=" <> paymentId.getId)
            pure resolved

      -- Stamped on the insert, not patched in afterwards: a status read hitting the replica in
      -- between would see a booking that is neither payable nor pass-covered and reject it.
      let mbPassOption = snd <$> mbResolved
      -- A reschedule carries the PARENT booking's pass override verbatim (same pass, same fare). It is
      -- copied from the parent here, NOT re-run through resolvePassOverride, which would drop the override
      -- once the parent has already spent the pass's last metered trip. mbPassOption stays Nothing on the
      -- reschedule path (the handler passes no pass id), so a non-pass reschedule copies the parent's empty
      -- override. The same-fare reschedule guard keeps the copied overriddenAmount correct for the new trip.
      mbOldBooking <- maybe (pure Nothing) (\ctx -> QFRFSTicketBooking.findById ctx.oldBookingId) mbRescheduleCtx
      let mbRescheduleCount = Just (maybe 0 (\b -> fromMaybe 0 b.rescheduleCount + 1) mbOldBooking)
          isReschedule = isJust mbRescheduleCtx
          bookingOverrideType = if isReschedule then mbOldBooking >>= (.overrideType) else DFRFSTicketBooking.PassOverride <$ mbPassOption
          bookingOverriddenAmount = if isReschedule then mbOldBooking >>= (.overriddenAmount) else (.overriddenTotalPrice.amount) <$> mbPassOption
          bookingOverrideAppliedEntityId = if isReschedule then mbOldBooking >>= (.overrideAppliedEntityId) else (.purchasedPassPaymentId.getId) <$> mbPassOption

      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                overrideType = bookingOverrideType,
                overriddenAmount = bookingOverriddenAmount,
                overrideAppliedEntityId = bookingOverrideAppliedEntityId,
                bppOrderId = Nothing,
                bppPaymentId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = quote'.merchantId,
                totalPrice = fareParameters.totalPrice,
                -- Reschedule pin: reuse the old payment (findTicketBookingPayment resolves it); Nothing on the normal path.
                frfsTicketBookingPaymentIdForTicketGeneration = (.getId) <$> (mbRescheduleCtx >>= (.oldFrfsPaymentId)),
                paymentTxnId = Nothing,
                bppBankAccountNumber = Nothing,
                bppBankCode = Nothing,
                cancellationCharges = Nothing,
                refundAmount = Nothing,
                isBookingCancellable = Nothing,
                customerCancelled = False,
                payerVpa = Nothing,
                cashbackPayoutOrderId = Nothing,
                cashbackStatus = if isJust quote.discountedTickets then Just DFTB.PENDING else Nothing,
                bppDelayedInterest = quote.bppDelayedInterest,
                journeyOnInitDone = Nothing,
                startTime = Just bookingStartTime,
                isFareChanged = Just isFareChanged,
                integratedBppConfigId = quote.integratedBppConfigId,
                googleWalletJWTUrl = Nothing,
                bookingAuthCode = crisSdkResponse <&> (.bookAuthCode),
                osType = crisSdkResponse <&> (.osType),
                osBuildVersion = crisSdkResponse <&> (.osBuildVersion),
                recentLocationId = mbSearch >>= (.recentLocationId),
                failureReason = Nothing,
                isSingleMode = isSingleMode,
                isMockPayment = mbMockPayment,
                finalBoardedVehicleNumber = Nothing,
                finalBoardedVehicleNumberSource = Nothing,
                finalBoardedVehicleServiceTierType = Nothing,
                finalBoardedDepotNo = Nothing,
                finalBoardedScheduleNo = Nothing,
                finalBoardedWaybillId = Nothing,
                conductorId = Nothing,
                driverId = Nothing,
                driverName = Nothing,
                driverMobileNumber = Nothing,
                seatSelectionType = mbSeatSelectionType,
                routeCode = mbRouteCode,
                routeName = mbRouteName,
                serviceTierType = mbServiceTierType,
                ondcOnInitReceived = Nothing,
                ondcOnInitReceivedAt = Nothing,
                holdId = mbHoldCtxForAll <&> (\(h, _, _) -> h),
                tripId = firstTripId,
                isSpotBooking = isSpotBooking',
                vehicleNumber = quote'.vehicleNumber <|> mbVehicleNumber,
                waybillNo = firstTripId <&> (fst . JourneyUtils.getWaybillNoAndTripNoFromTripId),
                parentBookingId = (.oldBookingId) <$> mbRescheduleCtx,
                rescheduleCount = mbRescheduleCount,
                fromStopIdx = mbHoldCtxForAll <&> (\(_, f, _) -> f),
                toStopIdx = mbHoldCtxForAll <&> (\(_, _, t) -> t),
                cloudType = cloudType,
                clientSdkVersion = rider.clientSdkVersion,
                clientBundleVersion = rider.clientBundleVersion,
                ..
              }
      QFRFSTicketBooking.create booking

      -- Update userBookedRouteShortName and userBookedBusServiceTierType from route_stations_json
      let mbBookedRouteShortName = mbFirstRouteStation <&> (.shortName)
      let mbBookedServiceTierType = mbServiceTierType
      when (isJust mbBookedRouteShortName && isJust mbBookedServiceTierType) $
        whenJust mbJourneyLeg $ \journeyLeg -> do
          whenJust mbBookedRouteShortName $ \bookedRouteShortName ->
            QRouteDetails.updateUserBookedRouteShortName (Just bookedRouteShortName) journeyLeg.id.getId
          QJourneyLeg.updateByPrimaryKey $ journeyLeg {DJL.userBookedBusServiceTierType = mbBookedServiceTierType}

      return (rider, booking)

    calculateDynamicSeatHoldTTL ::
      ( MonadFlow m,
        ServiceFlow m r,
        HasShortDurationRetryCfg r c,
        HasBAPMetrics m r
      ) =>
      Text -> -- tripId (format: waybillNo-tripNumber)
      Text ->
      DIBC.IntegratedBPPConfig ->
      Int ->
      m Int
    calculateDynamicSeatHoldTTL tripId routeCode integratedBPPConfig bufferTime = do
      let (waybillNo, tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId tripId
      mbSchedule <- withTryCatch "calculateDynamicSeatHoldTTL:getBusTripSchedule" (OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBPPConfig)
      case mbSchedule of
        Left err -> do
          logWarning $ "Failed to fetch bus trip schedule, falling back to bufferTime: " <> show err
          pure bufferTime
        Right [] -> do
          logWarning "Empty schedule returned from bus-trip-schedule"
          pure bufferTime
        Right schedule -> do
          now <- getCurrentTime
          let allEtas = concatMap (.eta) schedule
              firstStop = minimumBy (comparing (.arrivalTimeUnix)) allEtas
              tripStartTime = unixToUTC firstStop.arrivalTimeUnix
              timeUntilTrip = diffUTCTime tripStartTime now
              timeUntilTripSec =
                round (realToFrac timeUntilTrip :: Double)
              finalTtl =
                bufferTime + max 0 timeUntilTripSec
          logInfo $ "Dynamic TTL calculated: tripStart=" <> show tripStartTime <> " ttl=" <> show finalTtl
          pure finalTtl

    -- Resolve the scheduled departure time for a bus trip from the live waybill schedule.
    -- Prefers the rider's boarding stop (matched on stop code); falls back to the trip's
    -- earliest stop when the boarding stop is not present. Returns Nothing when the schedule
    -- is unavailable or empty so callers can fall back safely.
    getScheduledTripStartTime ::
      ( MonadFlow m,
        ServiceFlow m r,
        HasShortDurationRetryCfg r c,
        HasBAPMetrics m r
      ) =>
      Text -> -- tripId (format: waybillNo-tripNumber)
      Text -> -- routeCode
      Text -> -- boarding stop code
      DIBC.IntegratedBPPConfig ->
      m (Maybe UTCTime)
    getScheduledTripStartTime tripId routeCode boardingStopCode integratedBPPConfig = do
      let (waybillNo, tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId tripId
      mbSchedule <- withTryCatch "getScheduledTripStartTime:getBusTripSchedule" (OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBPPConfig)
      case mbSchedule of
        Left err -> do
          logWarning $ "getScheduledTripStartTime: failed to fetch bus trip schedule for tripId=" <> tripId <> ": " <> show err
          pure Nothing
        Right schedule ->
          case concatMap (.eta) schedule of
            [] -> do
              logWarning $ "getScheduledTripStartTime: empty schedule for tripId=" <> tripId
              pure Nothing
            allEtas -> do
              let mbBoardingEta = listToMaybe (filter (\e -> e.stopCode == boardingStopCode) allEtas)
                  chosenEta = fromMaybe (minimumBy (comparing (.arrivalTimeUnix)) allEtas) mbBoardingEta
              pure $ Just (unixToUTC chosenEta.arrivalTimeUnix)

postFrfsQuoteV2ConfirmUtil :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text], HasField "cloudType" r (Maybe CloudType), HasMasterCloudForwarder r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> DFRFSQuote.FRFSQuote -> [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq] -> Maybe CrisSdkResponse -> Maybe Bool -> Maybe Bool -> Maybe Bool -> DIBC.IntegratedBPPConfig -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe RescheduleCtx -> Maybe (Id DPPP.PurchasedPassPayment) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2ConfirmUtil (mbPersonId, merchantId_) quote selectedQuoteCategories crisSdkResponse isSingleMode mbEnableOffer mbIsMockPayment integratedBppConfig mbTripId isSpotBooking mbVehicleNumber mbRescheduleCtx mbPurchasedPassPaymentId = do
  when (null selectedQuoteCategories) $ throwError $ NoSelectedCategoryFound quote.id.getId
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  (rider, dConfirmRes, fareParameters, updatedQuoteCategories, isMultiInitAllowed) <- confirmAndUpsertBooking personId quote selectedQuoteCategories crisSdkResponse isSingleMode mbIsMockPayment integratedBppConfig mbTripId isSpotBooking mbVehicleNumber mbRescheduleCtx mbPurchasedPassPaymentId
  (mbJourneyId, _) <- getAllJourneyFrfsBookings dConfirmRes
  when (isNothing mbJourneyId) $ do
    when (isNothing mbRescheduleCtx) $ do
      fork "FRFS buildJourneyAndLeg" $ buildJourneyAndLeg dConfirmRes fareParameters
      fork "Caching recent location for FRFS booking" $ JourneyUtils.createRecentLocationForFRFSBooking dConfirmRes
    -- Sync vehicle/driver data synchronously (NOT forked). A forked sync races with the confirm/on_init writes,
    -- which rewrite the whole booking row in KV and clobber the driver fields. Running it inline lets the
    -- subsequent sequential KV updates re-read the row and carry these fields forward.
    syncFRFSBookingVehicleData dConfirmRes integratedBppConfig
  merchantOperatingCity <- getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< dConfirmRes.routeStationsJson
  now <- getCurrentTime
  let isFullyPassCovered = FRFSPassOverride.isFullyPassCovered dConfirmRes.overriddenAmount
  -- Only a standalone booking may confirm inline. A journey leg is deferred to
  -- SharedLogic.FRFSPassConfirm, driven either by the journey's payment success or -- when no leg is
  -- payable at all -- by Lib.JourneyModule.Base once every leg is confirmed. Confirming a leg here
  -- would issue a ticket and spend a pass trip before the rider has paid for the journey's other legs.
  if isFullyPassCovered && dConfirmRes.status == DFRFSTicketBooking.NEW && isNothing mbJourneyId
    then do
      bapConfig <-
        getOneConfig
          (BecknConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, merchantId = merchant.id.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType), becknProtocol = Nothing})
          (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType)))
          >>= fromMaybeM (InternalError "Beckn Config not found")
      let validTillPass = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
          mRiderNamePass = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
      mRiderNumberPass <- mapM decrypt rider.mobileNumber
      -- dConfirmRes.status in the guard above is a snapshot taken before confirmAndUpsertBooking's
      -- lock was released, so it cannot decide this on its own: two concurrent confirms for one
      -- searchId would both read NEW and both call the BPP, issuing two tickets and debiting the
      -- pass twice at on_confirm. claimBookingForConfirm re-reads under a lock and is the only
      -- thing allowed to make the NEW -> CONFIRMING transition -- the same claim the journey path
      -- uses, so the two cannot drift.
      mbClaimed <- FRFSUtils.claimBookingForConfirm dConfirmRes.id validTillPass
      case mbClaimed of
        Nothing -> logInfo $ "FRFSConfirm: pass-covered booking no longer NEW, skipping inline confirm bookingId=" <> dConfirmRes.id.getId
        Just claimedBooking -> do
          void $ QFRFSTicketBooking.updateOnInitDone (Just True) claimedBooking.id
          -- The booking is CONFIRMING by now. CallExternalBPP.confirm returns Left for an error it
          -- handled but THROWS on a transport or decode failure, and a throw here skips both writes
          -- below and escapes as a 5xx, leaving the booking CONFIRMING with an empty
          -- failure_reason. Nothing sweeps that: this branch requires isNothing mbJourneyId, so no
          -- CheckMultimodalConfirmFail job is scheduled, and with no payment row none of the
          -- payment-driven paths touch it either -- only the rider polling status past validTill
          -- would ever resolve it. Fold a throw into the same Left path instead.
          confirmResp <-
            withTryCatch "FRFSConfirm:passCoveredBppConfirm" (CallExternalBPP.confirm merchant merchantOperatingCity bapConfig (mRiderNamePass, mRiderNumberPass) claimedBooking updatedQuoteCategories isSingleMode) >>= \case
              Right resp -> pure resp
              Left err -> pure (Left ("BPP confirm threw: " <> show err))
          case confirmResp of
            Left err -> do
              void $ QFRFSTicketBooking.updateFailureReasonById (Just err) claimedBooking.id
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED claimedBooking.id
            Right _ -> pure ()
    else when isMultiInitAllowed $ do
      case mbRescheduleCtx of
        Just ctx -> do
          -- Load-bearing: rewrite the reused payment's categories to the new seats BEFORE the staging
          -- confirm reads them (confirm resolves seats from findAllByPaymentId first). A fully pass-covered
          -- reschedule has no reused payment (oldFrfsPaymentId = Nothing) and confirms via the pass path,
          -- so there is nothing to sync — only APPROVED still applies as the multi-init marker.
          whenJust ctx.oldFrfsPaymentId $ \oldFrfsPaymentId ->
            FRFSReschedule.syncPaymentCategories oldFrfsPaymentId updatedQuoteCategories
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.APPROVED dConfirmRes.id
        Nothing -> do
          bapConfig <- getOneConfig (BecknConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, merchantId = merchant.id.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType), becknProtocol = Nothing}) (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType))) >>= fromMaybeM (InternalError "Beckn Config not found")
          let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
          mRiderNumber <- mapM decrypt rider.mobileNumber
          let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
          void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
          let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
          when (dConfirmRes.status /= DFRFSTicketBooking.NEW) $ do
            void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.NEW dConfirmRes.id
          CallExternalBPP.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) dConfirmRes' updatedQuoteCategories mbEnableOffer
  frfsBookingStatus (dConfirmRes.riderId, merchantId_) (integratedBppConfig.platformType == DIBC.MULTIMODAL) (withPaymentStatusResponseHandler dConfirmRes updatedQuoteCategories fareParameters routeStations stations merchantOperatingCity) dConfirmRes rider (\_ _ -> pure ())
  where
    withPaymentStatusResponseHandler ::
      CallExternalBPP.FRFSConfirmFlow m r c =>
      DFRFSTicketBooking.FRFSTicketBooking ->
      [FRFSQuoteCategory.FRFSQuoteCategory] ->
      FRFSFareParameters ->
      Maybe [FRFSTicketService.FRFSRouteStationsAPI] ->
      [FRFSTicketService.FRFSStationAPI] ->
      DMOC.MerchantOperatingCity ->
      ((DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, Maybe DPayment.PaymentStatusResp) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) ->
      m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
    withPaymentStatusResponseHandler booking quoteCategories fareParameters routeStations stations merchantOperatingCity action = do
      mbPaymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findTicketBookingPayment booking
      mbPaymentOrder <- maybe (pure Nothing) (QPaymentOrder.findById . (.paymentOrderId)) mbPaymentBooking
      case (mbPaymentBooking, mbPaymentOrder) of
        (Just paymentBooking, Just paymentOrder) -> do
          action (paymentBooking, paymentOrder, Nothing)
        _ -> do
          latestBooking <- B.runInReplica $ QFRFSTicketBooking.findById booking.id >>= fromMaybeM (InvalidRequest "Invalid booking id")
          mbAppliedPassPayment <- FRFSPassOverride.paymentForOverrideAppliedEntity latestBooking.overrideAppliedEntityId
          return $ makeBookingStatusAPI (latestBooking, quoteCategories) fareParameters routeStations stations merchantOperatingCity.city mbAppliedPassPayment

    makeBookingStatusAPI (booking, quoteCategories) fareParameters routeStations stations city mbAppliedPassPayment = do
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          overrideType = booking.overrideType,
          overriddenTotalPrice = mkPriceAPIEntity . Kernel.Types.Common.mkPrice (Just booking.totalPrice.currency) <$> booking.overriddenAmount,
          appliedPurchasedPassPaymentId = Id <$> booking.overrideAppliedEntityId,
          appliedPassId = mbAppliedPassPayment >>= (.passId),
          appliedPassName = mbAppliedPassPayment >>= (.passName),
          startTime = booking.startTime,
          city,
          updatedAt = booking.updatedAt,
          createdAt = booking.createdAt,
          _type = booking._type,
          quoteCategories = map mkFRFSQuoteCategoryAPIEntity quoteCategories,
          price = Just booking.totalPrice.amount,
          priceWithCurrency = Just $ mkPriceAPIEntity booking.totalPrice,
          quantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity),
          validTill = booking.validTill,
          vehicleType = booking.vehicleType,
          status = booking.status,
          payment = Nothing,
          tickets = [],
          discountedTickets = booking.discountedTickets,
          eventDiscountAmount = booking.eventDiscountAmount,
          isFareChanged = booking.isFareChanged,
          googleWalletJWTUrl = booking.googleWalletJWTUrl,
          integratedBppConfigId = booking.integratedBppConfigId,
          bppOrderId = booking.bppOrderId,
          isSpotBooking = booking.isSpotBooking,
          ..
        }

-- | Sync live vehicle + driver/conductor data onto the frfs_ticket_booking row.
-- Runs synchronously (NOT forked) from the confirm flow. A forked version raced with the confirm/on_init
-- booking writes: KV updates rewrite the whole row, so a concurrent write from a stale snapshot clobbered the
-- driver fields in Redis (Postgres kept them, since the drainer applies column-scoped UPDATEs). Running inline
-- lets the subsequent sequential KV updates re-read and carry these fields forward.
syncFRFSBookingVehicleData ::
  ( HasBAPMetrics m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  DFTB.FRFSTicketBooking ->
  DIBC.IntegratedBPPConfig ->
  m ()
syncFRFSBookingVehicleData booking integratedBppConfig = do
  let mbRouteStations :: Maybe [FRFSTicketService.FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
      mbRouteStation = listToMaybe =<< mbRouteStations
  routeLiveInfo <-
    case (mbRouteStation, booking.vehicleNumber) of
      (Just routeStation, Just vehicleNumber) -> do
        eRouteLiveInfo <- withTryCatch "syncFRFSBookingVehicleData:getLiveRouteInfo" (JourneyUtils.getLiveRouteInfo integratedBppConfig vehicleNumber routeStation.code)
        case eRouteLiveInfo of
          Left err -> do
            logWarning $ "Failed to fetch live route info for vehicleNumber=" <> vehicleNumber <> ": " <> show err
            pure Nothing
          Right info -> pure info
      _ -> return Nothing
  -- Enrich driver/conductor from waybill metadata when live tracking lacks them
  mbWaybillMeta <-
    case booking.tripId of
      Nothing -> pure Nothing
      Just tripId -> do
        let (waybillNo, _) = JourneyUtils.getWaybillNoAndTripNoFromTripId tripId
        meta <- withTryCatch "syncFRFSBookingVehicleData:getWaybillMetadata" (OTPRest.getWaybillMetadata waybillNo integratedBppConfig)
        case meta of
          Left err -> do
            logWarning $ "Failed to fetch waybill metadata for waybillNo=" <> waybillNo <> ": " <> show err
            pure Nothing
          Right m -> pure $ Just m
  -- Best-effort enrichment: fall back to the booking's already-persisted values when live-tracking / waybill data
  -- is unavailable, so a missing lookup never erases previously-enriched columns (e.g. on an idempotent re-confirm).
  let effectiveDriverId = (mbWaybillMeta >>= (.driver_id)) <|> (routeLiveInfo >>= (.busDriverId)) <|> booking.driverId
      effectiveDriverName = (mbWaybillMeta >>= (.driverName)) <|> booking.driverName
      effectiveDriverMobileNumber = (mbWaybillMeta >>= (.driverMobileNumber)) <|> booking.driverMobileNumber
      effectiveConductorId = (routeLiveInfo >>= (.busConductorId)) <|> booking.conductorId
      effectiveFinalBoardedVehicleNumber = booking.vehicleNumber <|> booking.finalBoardedVehicleNumber
      effectiveFinalBoardedVehicleNumberSource = (routeLiveInfo <&> \_ -> DJL.UserSpotBooked) <|> booking.finalBoardedVehicleNumberSource
      effectiveFinalBoardedWaybillId = (routeLiveInfo >>= (.waybillId)) <|> booking.finalBoardedWaybillId
      effectiveFinalBoardedScheduleNo = (routeLiveInfo >>= (.scheduleNo)) <|> booking.finalBoardedScheduleNo
      effectiveFinalBoardedDepotNo = (routeLiveInfo >>= (.depot)) <|> booking.finalBoardedDepotNo
      effectiveFinalBoardedServiceTierType = (routeLiveInfo <&> (.serviceType)) <|> booking.finalBoardedVehicleServiceTierType
  QFRFSTicketBooking.updateFRFSTicketBookingVehicleDataById
    effectiveFinalBoardedVehicleNumber
    effectiveFinalBoardedVehicleNumberSource
    effectiveFinalBoardedWaybillId
    effectiveFinalBoardedScheduleNo
    effectiveFinalBoardedDepotNo
    effectiveFinalBoardedServiceTierType
    effectiveConductorId
    effectiveDriverId
    effectiveDriverName
    effectiveDriverMobileNumber
    booking.id

buildJourneyAndLeg ::
  ( HasBAPMetrics m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool,
    HasShortDurationRetryCfg r c
  ) =>
  DFTB.FRFSTicketBooking ->
  FRFSFareParameters ->
  m ()
buildJourneyAndLeg booking fareParameters = do
  Hedis.whenWithLockRedis mkBookingJourneyCreateKey 60 $ do
    integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking

    now <- getCurrentTime
    journeyGuid <- generateGUID
    journeyLegGuid <- generateGUID

    eDistanceAndDuration <- withTryCatch "buildJourneyAndLeg:getDistanceAndDuration" (getDistanceAndDuration booking.fromStationPoint booking.toStationPoint)
    distanceAndDuration <-
      case eDistanceAndDuration of
        Left err -> do
          logError $ "Failed to fetch distance/duration from OSRM, defaulting distance to 0: " <> show err
          pure Nothing
        Right r -> pure r
    let distance = fromMaybe (Distance 0 Meter) (fst <$> distanceAndDuration)
        duration = snd <$> distanceAndDuration

    fromLocationId <- generateGUID
    let fromLocation =
          DL.Location
            { id = fromLocationId,
              createdAt = now,
              updatedAt = now,
              lat = maybe 0.0 (.lat) booking.fromStationPoint,
              lon = maybe 0.0 (.lon) booking.fromStationPoint,
              address =
                DLA.LocationAddress
                  { street = Nothing,
                    door = Nothing,
                    city = Nothing,
                    state = Nothing,
                    country = Nothing,
                    building = Nothing,
                    areaCode = Nothing,
                    area = booking.fromStationAddress,
                    ward = Nothing,
                    placeId = Nothing,
                    instructions = Nothing,
                    title = Nothing,
                    extras = Nothing
                  },
              merchantId = Just booking.merchantId,
              merchantOperatingCityId = Just booking.merchantOperatingCityId
            }

    toLocationId <- generateGUID
    let toLocation =
          DL.Location
            { id = toLocationId,
              createdAt = now,
              updatedAt = now,
              lat = maybe 0.0 (.lat) booking.toStationPoint,
              lon = maybe 0.0 (.lon) booking.toStationPoint,
              address =
                DLA.LocationAddress
                  { street = Nothing,
                    door = Nothing,
                    city = Nothing,
                    state = Nothing,
                    country = Nothing,
                    building = Nothing,
                    areaCode = Nothing,
                    area = booking.toStationAddress,
                    ward = Nothing,
                    placeId = Nothing,
                    instructions = Nothing,
                    title = Nothing,
                    extras = Nothing
                  },
              merchantId = Just booking.merchantId,
              merchantOperatingCityId = Just booking.merchantOperatingCityId
            }

    let mbRouteStations :: Maybe [FRFSTicketService.FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
        mbRouteStation = listToMaybe =<< mbRouteStations
    mbTrip <-
      case mbRouteStation of
        Just routeStation -> OTPRest.getExampleTrip integratedBppConfig routeStation.code
        Nothing -> return Nothing
    let mbFromTripStop = mbTrip >>= \trip -> OTPRest.findTripStopByStopCode trip booking.fromStationCode
        mbToTripStop = mbTrip >>= \trip -> OTPRest.findTripStopByStopCode trip booking.toStationCode
        legStart = fromMaybe booking.createdAt booking.startTime
        mbLegEnd =
          ( do
              fromStop <- mbFromTripStop
              toStop <- mbToTripStop
              pure $ addUTCTime (fromIntegral (toStop.scheduledArrival - fromStop.scheduledArrival)) legStart
          )
            <|> (duration <&> \d -> addUTCTime (fromIntegral (getSeconds d)) booking.createdAt)

    let journey =
          DJ.Journey
            { id = journeyGuid,
              convenienceCost = 0,
              estimatedDistance = distance,
              estimatedDuration = duration,
              -- Stamped here, not patched later: this journey is built in a fork that runs after
              -- OnConfirm, so OnConfirm's mark finds no leg yet and skips a fully covered booking.
              isPaymentSuccess = if FRFSPassOverride.fullyCoveredByPass booking then Just True else Nothing,
              totalLegs = 1,
              modes = [mapVehicleCategoryToTripMode booking.vehicleType],
              searchRequestId = booking.searchId.getId, -- Note :: This is not SearchRequest Table's ID. Do not use it to Query SearchReqeust Anywhere in Application.
              merchantId = booking.merchantId,
              status = DJ.CONFIRMED,
              riderId = booking.riderId,
              startTime = Just legStart,
              endTime = mbLegEnd,
              merchantOperatingCityId = booking.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now,
              recentLocationId = booking.recentLocationId,
              isPublicTransportIncluded = Just True,
              isSingleMode = Just True,
              relevanceScore = Nothing,
              hasPreferredServiceTier = Nothing,
              hasPreferredTransitModes = Just False,
              fromLocation = fromLocation,
              toLocation = Just toLocation,
              paymentOrderShortId = Nothing,
              journeyExpiryTime = Nothing,
              hasStartedTrackingWithoutBooking = Nothing,
              skipCreateOrderCall = Nothing
            }

    journeyRouteDetailsId <- generateGUID
    let estimatedPrice = find (\priceItem -> priceItem.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice)

    routeLiveInfo <-
      case (mbRouteStation, booking.vehicleNumber) of
        (Just routeStation, Just vehicleNumber) -> JourneyUtils.getLiveRouteInfo integratedBppConfig vehicleNumber routeStation.code
        _ -> return Nothing

    -- Platform codes are only carried by trip-stop data (Station / route-stop-mapping lookups
    -- don't have them); read them from the example trip fetched above.
    let fromStopPlatformCode = mbFromTripStop >>= (.platformCode)
        toStopPlatformCode = mbToTripStop >>= (.platformCode)
        fromStopDetail =
          MultiModalStopDetails
            { stopCode = Just booking.fromStationCode,
              platformCode = fromStopPlatformCode,
              name = booking.fromStationName,
              gtfsId = Just booking.fromStationCode
            }
        toStopDetail =
          MultiModalStopDetails
            { stopCode = Just booking.toStationCode,
              platformCode = toStopPlatformCode,
              name = booking.toStationName,
              gtfsId = Just booking.toStationCode
            }

    let journeyLeg =
          DJL.JourneyLeg
            { id = journeyLegGuid,
              mode = mapVehicleCategoryToTripMode booking.vehicleType,
              groupCode = Nothing,
              startLocation = LatLngV2 fromLocation.lat fromLocation.lon,
              endLocation = LatLngV2 toLocation.lat toLocation.lon,
              distance = Just distance,
              duration = duration,
              agency = Just $ MultiModalAgency {name = integratedBppConfig.agencyKey, gtfsId = Just integratedBppConfig.feedKey},
              fromArrivalTime = Nothing,
              fromDepartureTime = Just legStart,
              toArrivalTime = mbLegEnd,
              toDepartureTime = Nothing,
              fromStopDetails = Just fromStopDetail,
              toStopDetails = Just toStopDetail,
              routeDetails =
                [ DRD.RouteDetails
                    { agencyGtfsId = Just integratedBppConfig.feedKey,
                      agencyName = Just integratedBppConfig.agencyKey,
                      alternateShortNames = [],
                      alternateRouteIds = Nothing,
                      endLocationLat = toLocation.lat,
                      endLocationLon = toLocation.lon,
                      frequency = Nothing,
                      fromArrivalTime = Nothing,
                      fromDepartureTime = Just legStart,
                      fromStopCode = Just booking.fromStationCode,
                      fromStopGtfsId = Just booking.fromStationCode,
                      fromStopName = booking.fromStationName,
                      fromStopPlatformCode = fromStopPlatformCode,
                      id = journeyRouteDetailsId,
                      journeyLegId = journeyLegGuid.getId,
                      legStartTime = Just legStart,
                      legEndTime = mbLegEnd,
                      routeCode = mbRouteStation <&> (.code),
                      routeColorCode = mbRouteStation >>= (.color),
                      routeColorName = mbRouteStation >>= (.color),
                      routeGtfsId = mbRouteStation <&> (.code),
                      routeLongName = mbRouteStation <&> (.longName),
                      routeShortName = mbRouteStation <&> (.shortName),
                      userBookedRouteShortName = Nothing,
                      startLocationLat = fromLocation.lat,
                      startLocationLon = fromLocation.lon,
                      subLegOrder = Just 1,
                      toArrivalTime = mbLegEnd,
                      toDepartureTime = Nothing,
                      toStopCode = Just booking.toStationCode,
                      toStopGtfsId = Just booking.toStationCode,
                      toStopName = booking.toStationName,
                      toStopPlatformCode = toStopPlatformCode,
                      trackingStatus = Nothing,
                      trackingStatusLastUpdatedAt = Just now,
                      merchantId = Just booking.merchantId,
                      merchantOperatingCityId = Just booking.merchantOperatingCityId,
                      createdAt = now,
                      updatedAt = now
                    }
                ],
              liveVehicleAvailableServiceTypes = Nothing,
              estimatedMinFare = estimatedPrice <&> (.amount),
              estimatedMaxFare = estimatedPrice <&> (.amount),
              merchantId = booking.merchantId,
              merchantOperatingCityId = booking.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now,
              legSearchId = Just booking.searchId.getId,
              legPricingId = Just booking.quoteId.getId,
              changedBusesInSequence = Nothing,
              finalBoardedBusNumber = booking.vehicleNumber,
              finalBoardedBusNumberSource = routeLiveInfo <&> \_ -> DJL.UserSpotBooked,
              boardingConfirmedDespiteDistance = Nothing,
              finalBoardedDepotNo = routeLiveInfo >>= (.depot),
              finalBoardedScheduleNo = routeLiveInfo >>= (.scheduleNo),
              finalBoardedWaybillId = routeLiveInfo >>= (.waybillId),
              finalBoardedBusServiceTierType = routeLiveInfo <&> (.serviceType),
              userBookedBusServiceTierType = mbRouteStation >>= (.vehicleServiceTier) <&> (._type),
              userPreferredServiceTier = Nothing,
              osmEntrance = Nothing,
              osmExit = Nothing,
              straightLineEntrance = Nothing,
              straightLineExit = Nothing,
              journeyId = journeyGuid,
              isDeleted = Just False,
              sequenceNumber = 0,
              multimodalSearchRequestId = Just booking.searchId.getId, -- Note :: This is not SearchRequest Table's ID. Do not use it to Query SearchReqeust Anywhere in Application.
              busLocationData = booking.busLocationData,
              busConductorId = routeLiveInfo >>= (.busConductorId),
              busDriverId = routeLiveInfo >>= (.busDriverId),
              busTagNumber = routeLiveInfo >>= (.busTagNumber),
              providerRouteId = Nothing
            }

    -- NOTE: vehicle/driver sync onto frfs_ticket_booking is done synchronously in postFrfsQuoteV2ConfirmUtil
    -- (see syncFRFSBookingVehicleData) so it isn't clobbered by concurrent confirm/on_init KV whole-row writes.
    QLocation.createMany [fromLocation, toLocation]
    QJourney.create journey
    QJourneyLeg.create journeyLeg
  where
    mkBookingJourneyCreateKey = "booking:journey:create:bookingId-" <> booking.id.getId

    mapVehicleCategoryToTripMode = \case
      Spec.BUS -> DTrip.Bus
      Spec.METRO -> DTrip.Metro
      Spec.SUBWAY -> DTrip.Subway

    getDistanceAndDuration :: (ServiceFlow m r) => Maybe LatLong -> Maybe LatLong -> m (Maybe (Distance, Seconds))
    getDistanceAndDuration (Just source) (Just destination) =
      runMaybeT $ do
        let req =
              GetDistancesReq
                { origins = NonEmpty.fromList [source],
                  destinations = NonEmpty.fromList [destination],
                  travelMode = Just Maps.CAR,
                  sourceDestinationMapping = Nothing,
                  distanceUnit = Meter
                }
        distances <- lift $ Maps.getMultimodalJourneyDistances booking.merchantId booking.merchantOperatingCityId Nothing req
        leastDistanceRoute <- hoistMaybe $ minimumByMay (\r1 r2 -> compare r1.distance r2.distance) (toList distances)
        pure (leastDistanceRoute.distanceWithUnit, leastDistanceRoute.duration)
    getDistanceAndDuration _ _ = return Nothing
