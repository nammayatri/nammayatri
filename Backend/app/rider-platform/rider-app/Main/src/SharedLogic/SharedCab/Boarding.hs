-- | Shared-cab boarding engine: code resolution, proximity ladder, re-bind and degraded boarding.
module SharedLogic.SharedCab.Boarding
  ( SharedCabBoardOutcome (..),
    BoardingSummary (..),
    boardingCodeMatches,
    UnknownCodeStep (..),
    unknownCodeStep,
    forceHonoured,
    forcedNoLocation,
    bindingUnmoved,
    boardingGate,
    cabProximity,
    tryBoardSharedCab,
    boardingResponse,
    firstUse,
  )
where

import qualified API.Types.UI.MultimodalConfirm as API
import Control.Applicative ((<|>))
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicket as DTicket
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Person as DPerson
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Common.FRFSJourneyUtils (VehicleProximity (..), riderNearVehicle)
import qualified Lib.JourneyModule.Location as JMLocation
import SharedLogic.SharedCab.Allocation.Types (parseLtsTimestamp)
import SharedLogic.SharedCab.Booking (isSharedCabBooking, partyOf, partySizes, withBookingLock)
import SharedLogic.SharedCab.BookingState (clearAlloc)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled)
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Lts as Lts
import SharedLogic.SharedCab.Plate (canonicalisePlate, findByPlate, samePlate)
import qualified SharedLogic.SharedCab.RateLimit as RateLimit
import qualified SharedLogic.SharedCab.Schedule as Schedule
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error

data SharedCabBoardOutcome
  = -- | Boarding committed: ticket INPROGRESS, booking pointed at the cab, event emitted.
    SharedCabBoarded BoardingSummary
  | -- | the typed code matched no ACTIVE cab and the rider confirmed (forceCheckIn) —
    SharedCabDegraded
  | -- | Soft proximity miss: caller turns this into `boardingConfirmationRequired = True`
    SharedCabProximityHold (Maybe Double) Text
  deriving (Show, Generic)

data BoardingSummary = BoardingSummary
  { boardedSession :: Session,
    reboundFrom :: Maybe Session,
    boardedWithoutLocation :: Bool
  }
  deriving (Show, Generic)

boardingCodeMatches :: Text -> Text -> Bool
boardingCodeMatches code plate
  | T.length code <= 4 = canonicalisePlate code `T.isSuffixOf` canonicalisePlate plate
  | otherwise = samePlate code plate

-- | An unknown code never boards on its own (a typo would consume the ticket); the rider confirms an unlisted cab via forceCheckIn.
data UnknownCodeStep = AskToConfirmUnlisted | BoardUnlisted
  deriving (Show, Eq)

unknownCodeStep :: Maybe Bool -> UnknownCodeStep
unknownCodeStep forceCheckIn
  | forceCheckIn == Just True = BoardUnlisted
  | otherwise = AskToConfirmUnlisted

-- | forceCheckIn skips proximity for the allocated cab and for a booking with no cab yet; never for a re-bind.
forceHonoured :: Maybe Bool -> Maybe Text -> Text -> Bool
forceHonoured forceCheckIn bookingPlate targetPlate =
  forceCheckIn == Just True && maybe True (== targetPlate) bookingPlate

-- | A forced boarding with no cab yet counts as a no-location boarding, so a driver feeding codes to remote riders shows on the vehicle's counter.
forcedNoLocation :: Maybe Text -> Bool
forcedNoLocation = isNothing

-- | The cab resolved pre-lock must still be the booking's under the lock; a tick that claimed another cab in between makes the boarding stale.
bindingUnmoved :: Text -> Maybe Text -> Maybe Text -> Bool
bindingUnmoved boardingPlate pre fresh = pre == fresh || (isNothing pre && fresh == Just boardingPlate)

candidateRoutes :: DBooking.FRFSTicketBooking -> [Text]
candidateRoutes booking =
  nub $
    maybeToList booking.routeCode
      ++ foldMap (either (const []) (: []) . returnRouteOf) booking.routeCode

resolveCabByCode :: DBooking.FRFSTicketBooking -> Text -> Environment.Flow [Session]
resolveCabByCode booking code = do
  sessions <- concat <$> mapM Session.activeSessionsOnRoute (candidateRoutes booking)
  let candidates = filter (\s -> boardingCodeMatches code s.vehicleNumber) sessions
  catMaybes <$> mapM serveCheck candidates
  where
    serveCheck s = do
      ok <- sessionServesBoardDrop booking s
      pure $ if ok then Just s else Nothing

sessionServesBoardDrop :: DBooking.FRFSTicketBooking -> Session -> Environment.Flow Bool
sessionServesBoardDrop booking session = do
  integratedBppConfig <- CQIBC.findById session.integratedBppConfigId >>= fromMaybeM BoardingFailed
  stops <- OTPRest.getRouteStopMappingByRouteCode session.routeCode integratedBppConfig
  let seqOf c = (.sequenceNum) <$> find (\s -> s.stopCode == c) stops
  pure $ case (seqOf booking.fromStationCode, seqOf booking.toStationCode) of
    (Just fromSeq, Just toSeq) -> fromSeq < toSeq
    _ -> False

lookupCabPing :: Session -> Environment.Flow (Maybe (UTCTime, LatLong))
lookupCabPing session = do
  outcome <- withTryCatch "lookupCabPing" $ Lts.readRoutePositions session.routeCode
  pure $ case outcome of
    Left _ -> Nothing -- LTS outage behaves identically to NO_LIVE_CAB_DATA (8.2 ladder floor)
    Right tracked -> do
      v <- findByPlate (.vehicleNumber) session.vehicleNumber tracked
      pingTime <- parseLtsTimestamp =<< v.vehicleInfo.timestamp
      pure (pingTime, LatLong v.vehicleInfo.latitude v.vehicleInfo.longitude)

-- | The bus boarding match against the cab's ping, or against the boarding stop (as if pinged now) when the cab has no fresh ping;
-- Nothing when there is neither.
cabProximity :: UTCTime -> NominalDiffTime -> Maybe (UTCTime, LatLong) -> Maybe LatLong -> [API.RiderLocationReq] -> Maybe VehicleProximity
cabProximity now maxPingAge mbPing mbStop riderPoints =
  mfilter (/= StaleVehiclePing) (near <$> mbPing) <|> (near . (now,) <$> mbStop)
  where
    near ping = riderNearVehicle now maxPingAge riderPointMaxAge ping riderPoints

-- | Only the cab's ping takes the bus bar: the rider app's location cadence during cab boarding is not known to be under it.
riderPointMaxAge :: NominalDiffTime
riderPointMaxAge = 300

-- | Left -> proximity hold; Right -> pass. Ladder: journey location stream, then request lat/lon (pending a MultiModal.yaml
-- addition), then no location: allocated cab / direct board pass flagged, re-bind is a hard fail.
checkRiderNearSharedCab ::
  Config.SharedCabTunables ->
  Session ->
  DJourney.Journey ->
  DBooking.FRFSTicketBooking ->
  Bool -> -- isRebind
  Environment.Flow (Either (Maybe Double, Text) (Maybe Double, Bool))
checkRiderNearSharedCab tunables session journey booking isRebind = do
  riderHistory <- JMLocation.getAllPoints journey.id
  mbPing <- lookupCabPing session
  now <- getCurrentTime
  case cabProximity now (realToFrac tunables.boardPingMaxAgeSec) mbPing booking.fromStationPoint riderHistory of
    Nothing -> pure $ Left (Nothing, "NO_CAB_POSITION")
    Just (RiderDistance d)
      | d <= fromIntegral tunables.boardProximityM -> pure $ Right (Just d, False)
      | otherwise -> pure $ Left (Just d, "TOO_FAR_FROM_CAB")
    Just _
      | isRebind -> throwError BoardingLocationRequired -- hard fail
      | otherwise -> pure $ Right (Nothing, True)

-- | An unknown code is NEVER blocked: ticket -> INPROGRESS with a degrade marker, no driver card, no seat effect. Decided on a fresh read
-- under the booking lock; an allocated booking gives its cab back first so a degraded ride never counts as a seat.
degradedBoarding :: Int -> DBooking.FRFSTicketBooking -> Text -> Environment.Flow ()
degradedBoarding degradedTimeoutSec booking typedCode = do
  withBookingLock booking.id $ do
    fresh <- QBooking.findById booking.id >>= fromMaybeM BoardingFailed
    tickets <- QTicket.findAllByTicketBookingId booking.id
    markerAlive <- Degraded.isMarkerAlive booking.id
    mbAllocatedPlate <- Degraded.planDegrade fresh.status fresh.vehicleNumber (map (.status) tickets) markerAlive & fromMaybeM BoardingFailed
    whenJust mbAllocatedPlate $ \plate -> do
      QBooking.updateAllocatedVehicle Nothing booking.id (Just plate)
      clearAlloc booking.id
      Seats.releaseBooking booking.id
      -- Skips Allocation.closeLocked, so it emits allocation_closed {degraded, none} itself in the same locked block.
      now <- getCurrentTime
      Events.emit booking.merchantOperatingCityId $
        Events.bookingEvent (Events.AllocationClosed "DEGRADED" Events.BlameNone) booking.id.getId (Just plate) booking.routeCode now
    forM_ (filter ((== TicketStatus.ACTIVE) . (.status)) tickets) $ \t ->
      QTicket.updateStatusByTBookingIdAndTicketNumber TicketStatus.INPROGRESS t.scannedByVehicleNumber booking.id t.ticketNumber
    Degraded.markDegradedBoarding degradedTimeoutSec booking.id typedCode
    QBooking.updateUpdatedAtById booking.id
    Events.forBooking Events.DegradedBoarding booking
  -- Outside the booking lock; a failure must not fail a boarding that already committed (the retry would be refused: marker alive).
  withTryCatch "sharedCabSeedSweep" (Schedule.ensureDegradedSweep booking.merchantId booking.merchantOperatingCityId) >>= \case
    Left e -> logError $ "sharedCab: couldn't seed the degraded sweep for booking " <> booking.id.getId <> ": " <> show e
    Right () -> pure ()

-- Lock order when two locks are needed: target-cab plate lock first, then the booking lock.

-- | The boarding's decision under both locks, pure: the cab is still running, the booking is still live and the cab is still the one
-- it was resolved against. The tickets to flip are the booking's ACTIVE or INPROGRESS ones. The seats are the seat engine's call.
boardingGate :: Text -> Session -> [TicketStatus.FRFSTicketStatus] -> Maybe Text -> DBooking.FRFSTicketBooking -> Either SharedCabBoardingError ()
boardingGate plate fresh eligible resolvedAgainst freshBooking
  | fresh.status /= ACTIVE = Left BoardingFailed
  | null eligible = Left BoardingFailed
  | freshBooking.status /= DBookingStatus.CONFIRMED = Left BoardingFailed
  | not (bindingUnmoved plate resolvedAgainst freshBooking.vehicleNumber) = Left BoardingFailed
  | otherwise = Right ()

-- | The tickets a boarding flips: the booking's ACTIVE or INPROGRESS ones.
boardableTickets :: [DTicket.FRFSTicket] -> [DTicket.FRFSTicket]
boardableTickets myTickets = [t | t <- myTickets, t.status `elem` [TicketStatus.ACTIVE, TicketStatus.INPROGRESS]]

commitBoarding ::
  DJourneyLeg.JourneyLeg ->
  DBooking.FRFSTicketBooking ->
  Session -> -- target cab
  Maybe Session -> -- old cab, iff re-bind
  Environment.Flow ()
commitBoarding journeyLeg booking target mbOld =
  let plate = canonicalisePlate target.vehicleNumber
   in Session.withPlateLock plate $
        withBookingLock booking.id $ do
          -- Re-validate under locks. readSession, not getSession: getSession may rebuild under this same non-re-entrant plate lock.
          fresh <- Redis.withMasterRedis (Session.readSession plate) >>= fromMaybeM BoardingFailed
          myTickets <- QTicket.findAllByTicketBookingId booking.id
          freshBooking <- QBooking.findById booking.id >>= fromMaybeM BoardingFailed
          let eligible = boardableTickets myTickets
          -- a rider who types the cab's code is in the cab: boarding an offered booking accepts it (applyBoarding clears the offer)
          either throwError pure $ boardingGate plate fresh (map (.status) eligible) booking.vehicleNumber freshBooking
          -- SEAT GUARD: the party's seats on this cab for its ride, held in the seat engine (CabFull if they aren't free). Seats it
          -- already holds here are kept; seats held on another cab are given back once the boarding has landed.
          prior <- Seats.bookingHold booking.id
          if Seats.heldOn fresh prior
            then applyBoarding journeyLeg booking freshBooking fresh eligible mbOld
            else do
              parties <- partySizes [freshBooking]
              held <- Seats.holdForStops fresh freshBooking.fromStationCode freshBooking.toStationCode (partyOf parties freshBooking) >>= fromMaybeM CabFull
              applyBoarding journeyLeg booking freshBooking fresh eligible mbOld `onException` Seats.releaseHeld held
              Seats.recordHold booking.id held
              traverse_ Seats.releaseHeld prior

-- | The writes of a boarding the gate allowed, in order: tickets, then the booking's cab (CAS), the old cab's keys, the leg, the event.
applyBoarding :: DJourneyLeg.JourneyLeg -> DBooking.FRFSTicketBooking -> DBooking.FRFSTicketBooking -> Session -> [DTicket.FRFSTicket] -> Maybe Session -> Environment.Flow ()
applyBoarding journeyLeg booking freshBooking fresh eligible mbOld = do
  -- Ticket FIRST: INPROGRESS, never postFrfsTicketVerify (USED reads as leg completed). Flip ONLY the eligible set.
  forM_ eligible $ \t ->
    QTicket.updateStatusByTBookingIdAndTicketNumber TicketStatus.INPROGRESS (Just fresh.vehicleNumber) booking.id t.ticketNumber
  -- CAS vehicleNumber against the fresh read.
  QBooking.updateAllocatedVehicle (Just fresh.vehicleNumber) booking.id freshBooking.vehicleNumber
  QBooking.updateVehicleTripId (Just fresh.vehicleTripId) booking.id
  -- Old cab's seat release is derived: no consecutiveMisses/attempts bump and no allocation_closed{blame: driver}.
  clearAlloc booking.id
  Degraded.clearDegradedMarker booking.id
  -- In the lock, not forked: a fork could land after a later re-bind and write the old cab back.
  QBooking.updateFRFSTicketBookingVehicleDataById
    (Just fresh.vehicleNumber)
    (Just DJourneyLeg.UserActivated)
    Nothing -- waybill: bus-fleet concept, cabs have none
    Nothing -- scheduleNo
    Nothing -- depot
    booking.serviceTierType
    Nothing -- conductorId
    (Just fresh.driverId)
    Nothing -- driverName: join driver-app later
    Nothing -- driverMobileNumber
    booking.id
  QJourneyLeg.updateByPrimaryKey $
    journeyLeg
      { DJourneyLeg.finalBoardedBusNumber = Just fresh.vehicleNumber,
        DJourneyLeg.finalBoardedBusNumberSource = Just DJourneyLeg.UserActivated,
        DJourneyLeg.finalBoardedBusServiceTierType = booking.serviceTierType
      }
  -- NOTE: BusBoardingMethod has no SHARED_CAB value; UserActivated is the closest fit.
  now <- getCurrentTime
  let event k = Events.withTrip (Just fresh.vehicleTripId.getId) $ Events.bookingEvent k booking.id.getId (Just fresh.vehicleNumber) (Just fresh.routeCode) now
  Events.emit fresh.merchantOperatingCityId . event $ case mbOld of
    Nothing -> Events.Boarded Events.ByCode
    Just old -> Events.Rebound old.vehicleNumber (old.routeCode /= fresh.routeCode)

-- | The SHARED_CAB branch; `Nothing` means "not our tier" and the caller falls through to the bus/metro body.
tryBoardSharedCab ::
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  DBooking.FRFSTicketBooking ->
  Maybe (Id DPerson.Person) ->
  API.OnboardedVehicleDetailsReq ->
  Environment.Flow (Maybe SharedCabBoardOutcome)
tryBoardSharedCab _ _ booking _ _
  | not (isSharedCabBooking booking) = pure Nothing
tryBoardSharedCab journey journeyLeg booking mbPersonId req =
  requireSharedCabEnabled booking.merchantOperatingCityId >> boardSharedCab journey journeyLeg booking mbPersonId req

-- | A boarding that puts a booking's tickets in use for the first time (the bus counts its booking used at check-in, so a cab does at boarding).
firstUse :: [TicketStatus.FRFSTicketStatus] -> SharedCabBoardOutcome -> Bool
firstUse ticketsBefore outcome = boarded && not (any (`elem` [TicketStatus.INPROGRESS, TicketStatus.USED]) ticketsBefore)
  where
    boarded = case outcome of
      SharedCabBoarded _ -> True
      SharedCabDegraded -> True
      SharedCabProximityHold _ _ -> False

boardingResponse :: SharedCabBoardOutcome -> API.JourneyInfoResp -> API.JourneyInfoResp
boardingResponse outcome journeyInfoResp = case outcome of
  SharedCabProximityHold holdDistanceMeters holdReason ->
    journeyInfoResp
      { API.boardingConfirmationRequired = Just True,
        API.boardingDistanceFromBusMeters = holdDistanceMeters,
        API.boardingConfirmationReason = Just holdReason
      }
  SharedCabBoarded _ -> journeyInfoResp
  SharedCabDegraded -> journeyInfoResp

boardSharedCab ::
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  DBooking.FRFSTicketBooking ->
  Maybe (Id DPerson.Person) ->
  API.OnboardedVehicleDetailsReq ->
  Environment.Flow (Maybe SharedCabBoardOutcome)
boardSharedCab journey journeyLeg booking mbPersonId req = do
  personId <- mbPersonId & fromMaybeM BoardingFailed
  unless (booking.riderId == personId) $ throwError BoardingFailed
  unless (booking.status == DBookingStatus.CONFIRMED) $ throwError BoardingFailed
  tunables <- Config.getTunables booking.merchantOperatingCityId
  RateLimit.enforceBoardingAttemptLimit tunables.boardAttemptsPer10Min booking.id
  code <- req.vehicleNumber & fromMaybeM BoardingFailed
  matches <- resolveCabByCode booking code
  case matches of
    [] -> case unknownCodeStep req.forceCheckIn of
      AskToConfirmUnlisted -> pure . Just $ SharedCabProximityHold Nothing "UNLISTED_CAB"
      BoardUnlisted -> degradedBoarding tunables.degradedTimeoutSec booking code >> pure (Just SharedCabDegraded)
    [target] -> Just <$> boardMatched tunables target
    _ -> throwError BoardingCodeAmbiguous -- two match: ask for the full plate
  where
    boardMatched tunables target = do
      let isAllocatedCab = booking.vehicleNumber == Just target.vehicleNumber
          isRebind = isJust booking.vehicleNumber && not isAllocatedCab
          forced = forceHonoured req.forceCheckIn booking.vehicleNumber target.vehicleNumber
      mbCase <-
        if forced
          then pure $ Right (Nothing, forcedNoLocation booking.vehicleNumber)
          else checkRiderNearSharedCab tunables target journey booking isRebind
      case mbCase of
        Left (mbDist, reason) -> pure $ SharedCabProximityHold mbDist reason
        Right (_mbDist, noLocation) -> do
          mbOld <-
            if isRebind
              then join <$> traverse Session.getSession booking.vehicleNumber
              else pure Nothing
          commitBoarding journeyLeg booking target mbOld
          -- no rider location counts against the VEHICLE's IST-day counter, so a driver feeding codes to non-app riders shows up per vehicle
          when noLocation $
            RateLimit.recordNoLocationBoarding tunables.noLocationSpotBookingsPerVehiclePerDay (canonicalisePlate target.vehicleNumber)
          -- TODO: re-fare if the re-bound route differs.
          pure $
            SharedCabBoarded
              BoardingSummary
                { boardedSession = target,
                  reboundFrom = mbOld,
                  boardedWithoutLocation = noLocation
                }
