module Domain.Action.UI.SharedCab
  ( getSharedCabRoutes,
    postSharedCabBookingSkip,
    skipReason,
  )
where

import qualified API.Types.UI.SharedCab as API
import Data.List (sortOn)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (CONFIRMED))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as DRoute
import qualified Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation as Allocation
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import SharedLogic.SharedCab.Allocation.Types (SkipReason (..), usesRiderSkip)
import qualified SharedLogic.SharedCab.Approach as Approach
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import SharedLogic.SharedCab.BookingState (readRiderSkipUsed)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.LegState (SkipVerdict (..), decideRiderSkip)
import qualified SharedLogic.SharedCab.Session as Session
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
