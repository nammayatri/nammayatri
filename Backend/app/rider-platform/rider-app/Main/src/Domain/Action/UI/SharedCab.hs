module Domain.Action.UI.SharedCab
  ( getSharedCabRoutes,
    getSharedCabRoute,
    postSharedCabBookingSkip,
    skipReason,
    liveCabEntry,
  )
where

import qualified API.Types.UI.SharedCab as API
import Control.Applicative ((<|>))
import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (CONFIRMED))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as DRoute
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SharedCab.Allocation as Allocation
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import SharedLogic.SharedCab.Allocation.Types (SkipReason (..), usesRiderSkip)
import qualified SharedLogic.SharedCab.Approach as Approach
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import SharedLogic.SharedCab.BookingState (readRiderSkipUsed)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Demand as Demand
import SharedLogic.SharedCab.Interest (bestEffort)
import SharedLogic.SharedCab.LegState (SkipVerdict (..), decideRiderSkip)
import qualified SharedLogic.SharedCab.Nearby as Nearby
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session)
import SharedLogic.SharedCab.WalkUp (findSharedCabConfig)
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error

mkRouteInfo :: DIBC.IntegratedBPPConfig -> DRoute.Route -> Environment.Flow API.SharedCabRouteInfo
mkRouteInfo integratedBppConfig route = do
  stops <- sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode route.code integratedBppConfig
  cabsRunning <- length <$> Session.activeSessionsOnRoute route.code
  pure
    API.SharedCabRouteInfo
      { routeCode = route.code,
        name = route.longName,
        stops = [API.SharedCabStop {code = s.stopCode, name = s.stopName, point = s.stopPoint, sequenceNum = s.sequenceNum} | s <- stops],
        polyline = route.polyline,
        cabsRunning
      }

getSharedCabRoutes :: (Maybe (Id DP.Person), Id DM.Merchant) -> Environment.Flow API.SharedCabRouteListResp
getSharedCabRoutes (mbPersonId, _) = do
  routes <-
    findSharedCabConfig mbPersonId >>= \case
      Nothing -> pure []
      Just integratedBppConfig -> OTPRest.getRoutesByGtfsIdWithPolyline integratedBppConfig >>= mapM (mkRouteInfo integratedBppConfig)
  pure API.SharedCabRouteListResp {routes}

-- | A failed LTS read means no cab is seen on the route, not a failed list.
liveCabsOnRoute :: Text -> Environment.Flow [LT.VehicleTrackingOnRouteResp]
liveCabsOnRoute routeCode =
  withTryCatch "sharedCab:vehicleTrackingOnRoute" (LF.vehicleTrackingOnRoute (LF.ByRoute routeCode)) >>= \case
    Left err -> [] <$ logError ("shared-cab route " <> routeCode <> ": LTS read failed: " <> show err)
    Right cabs -> pure cabs

-- | What the rider sees of a cab. The plate stays server-side: the rider names a cab by `cabId` and sees only the last four characters.
-- `freeSeats` and `ridersOnBoard` are the seat engine's (Seats.cabLoad).
liveCabEntry :: (Int, Int) -> Maybe Int -> Maybe Int -> Maybe LatLong -> Session -> API.SharedCabLiveCab
liveCabEntry (freeSeats, ridersOnBoard) etaMin distanceM position s =
  API.SharedCabLiveCab
    { cabId = s.vehicleTripId.getId,
      plateLast4 = T.takeEnd 4 s.vehicleNumber,
      position,
      freeSeats,
      ridersOnBoard,
      etaMin,
      distanceM
    }

-- | With `boardStop` each cab says how soon it reaches that stop and how far it is (from the rider when `lat`/`lon` are given, else from the
-- stop); cabs are listed soonest first. `pickEnabled` tells the app whether the city lets the rider pick one.
getSharedCabRoute :: (Maybe (Id DP.Person), Id DM.Merchant) -> Text -> Maybe Text -> Maybe Double -> Maybe Double -> Environment.Flow API.SharedCabRouteDetailResp
getSharedCabRoute (mbPersonId, _) routeCode mbBoardStop mbLat mbLon = do
  integratedBppConfig <- findSharedCabConfig mbPersonId >>= fromMaybeM IntegratedBPPConfigNotFound
  route <- OTPRest.getRouteByRouteId integratedBppConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)
  routeInfo <- mkRouteInfo integratedBppConfig route
  cfg <- Config.getTunables integratedBppConfig.merchantOperatingCityId
  now <- getCurrentTime
  -- opening the cab list for a boarding stop is a search for a cab there
  forM_ ((,) <$> mbPersonId <*> mbBoardStop) $ \(personId, stopCode) ->
    void $ bestEffort (logError . ("shared-cab search not recorded: " <>)) (Demand.recordSearch integratedBppConfig.merchantOperatingCityId.getId personId.getId stopCode)
  sessions <- Session.activeSessionsOnRoute routeCode
  live <- liveCabsOnRoute routeCode
  let positions = Map.fromList [(canonicalisePlate v.vehicleNumber, v) | v <- live]
      boardStop = mbBoardStop >>= \code -> find (\st -> st.code == code) routeInfo.stops
      reference = (LatLong <$> mbLat <*> mbLon) <|> ((.point) <$> boardStop)
  cabs <- forM sessions $ \s -> do
    let mbLive = Map.lookup s.vehicleNumber positions
        position = (\v -> LatLong v.vehicleInfo.latitude v.vehicleInfo.longitude) <$> mbLive
        eta = join $ (\st -> Nearby.nextCabEtaMin now cfg st.code st.point (maybeToList mbLive)) <$> boardStop
    load <- Seats.cabLoad s mbBoardStop ((.vehicleInfo) <$> mbLive)
    pure (eta, liveCabEntry load eta (round <$> (distanceBetweenInMeters <$> reference <*> position)) position s)
  pure API.SharedCabRouteDetailResp {routeInfo, cabs = Nearby.sortByEta cabs, pickEnabled = cfg.pickEnabled}

skipReason :: API.SharedCabSkipReason -> SkipReason
skipReason = \case
  API.FULL -> SkipFull
  API.OTHER -> SkipOther

-- | "Skip this cab": once per booking, only while the allocated cab is still coming and further than skipMinEtaSec from the board stop
-- (never after boarding). The booking goes back to FINDING, that cab isn't offered to it again, and nobody is charged.
postSharedCabBookingSkip :: (Maybe (Id DP.Person), Id DM.Merchant) -> Id DFTB.FRFSTicketBooking -> API.SharedCabSkipReq -> Environment.Flow APISuccess
postSharedCabBookingSkip (mbPersonId, _) bookingId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Booking not found")
  unless (booking.riderId == personId && isSharedCabBooking booking && booking.status == CONFIRMED) $
    throwError $ InvalidRequest "Booking not found"
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  let reason = skipReason req.reason
  used <- readRiderSkipUsed booking.id
  -- the one-skip and distance rules are for the rider's own choice; "it arrived full" is the cab's fault and is never limited
  when (usesRiderSkip reason) $ do
    when used $ throwError SharedCabSkipUsed
    when (DFRFSTicket.INPROGRESS `elem` map (.status) tickets) $ throwError SharedCabSkipTooLate
  plate <- Select.allocatedPlate (Select.LiveBooking booking (map (.status) tickets)) & fromMaybeM (InvalidRequest "This booking has no cab to skip")
  cfg <- Config.getTunables booking.merchantOperatingCityId
  when (usesRiderSkip reason) $ do
    now <- getCurrentTime
    mbEta <- maybe (pure Nothing) (\routeCode -> Approach.etaToStopSec now cfg routeCode plate booking.fromStationCode) booking.routeCode
    when (decideRiderSkip False False mbEta /= SkipOk) $ throwError SharedCabSkipTooLate
  skipped <- Allocation.skipSharedCabAllocation cfg booking.id plate reason
  unless skipped $ throwError $ InvalidRequest "This booking has no cab to skip"
  pure Success
