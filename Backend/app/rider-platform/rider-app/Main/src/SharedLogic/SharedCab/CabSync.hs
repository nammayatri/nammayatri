-- | Rider-app's side of a shared cab's run, which driver-app owns. 'applyCabUpdate' writes the copy driver-app sends (its sync on every
-- change and session poll, or its answer to a 'cabEvent') and runs the change's effects on the bookings: a route change or a pause
-- releases the unboarded riders, an end drops the boarded ones. 'cabEvent' tells driver-app what rider-app saw (silence, off route,
-- the last rider dropped). Rider-app's plate lock is never held across a call to driver-app.
module SharedLogic.SharedCab.CabSync
  ( CabSyncFlow,
    applyCabUpdate,
    cabEvent,
    cabEmptied,
    cabRiderOf,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.SharedCabSession (CabChange (..), CabEvent (..), CabRider, CabUpdate)
import qualified Domain.Types.SharedCabSession as SCS
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow)
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (RouteChanged, SessionClosed))
import SharedLogic.SharedCab.Booking (ridersOnBoard)
import qualified SharedLogic.SharedCab.Booking as Booking
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QPerson
import Tools.Error

type CabSyncFlow m r = (AllocFlow m r, HasRequestId r)

-- | The copy as written (or the one held, for an update older than it), and the effects of the change outside the plate lock.
applyCabUpdate :: CabSyncFlow m r => CabUpdate -> m Session
applyCabUpdate update = do
  let cab = update.cab
      plate = cab.vehicleNumber
  ibc <- SIBC.findSharedCabConfig cab.agencyId DIBC.APPLICATION
  (prior, next, applied) <- Session.withPlateLock plate $ do
    prior <- Session.readSession plate
    case prior of
      Just p | isStale cab p -> pure (prior, p, False)
      _ -> do
        let next = copyFrom ibc.merchantId ibc.merchantOperatingCityId ibc.id cab prior
        when (update.change == Just CAB_ENDED) $ dropOnBoard plate
        when (update.change == Just CAB_ROUTE_CHANGED) $ do
          carried <- Seats.carryOver next
          when (carried /= next.walkupCount) $ logError $ "sharedCab: " <> show carried <> " of " <> show next.walkupCount <> " walk-ups carried onto " <> next.vehicleTripId
        -- a copy rider-app had not seen (a flush, or a cab first seen now): its live bookings re-seed the plate's index
        when (maybe True (\p -> p.status == ENDED) prior) $
          Booking.liveBookingsForVehicle plate >>= mapM_ (Booking.noteOnPlate plate . (.id))
        saved <- Session.saveSession prior next
        pure (prior, saved, True)
  when applied $ case update.change of
    Just CAB_OPENED -> do
      Session.setCabDriverInfo plate Session.CabDriverInfo {Session.driverName = cab.driverName, Session.vehicleModel = cab.vehicleModel}
      -- a driver who opens on another route without ending the last run leaves its unboarded riders behind
      when (maybe False (\p -> p.status /= ENDED && p.routeCode /= next.routeCode) prior) $ Allocation.releaseUnboarded plate RouteChanged
    Just CAB_ROUTE_CHANGED -> do
      when update.notifyOnBoard $ ridersOnBoard plate >>= mapM_ (Notify.notifyRouteChange next.routeCode)
      Allocation.releaseUnboarded plate RouteChanged
    Just CAB_PAUSED -> Allocation.releaseUnboarded plate SessionClosed
    Just CAB_ENDED -> do
      Session.clearCabDriverInfo plate
      Allocation.releaseUnboarded plate SessionClosed
    _ -> pure ()
  pure next

-- | Every rider still on board is dropped when the cab ends. Each drop is caught on its own: one lock timeout must not strand the
-- others or stop the end itself; 'Session.dropStrandedRider' ends the ones left.
dropOnBoard :: CabSyncFlow m r => Text -> m ()
dropOnBoard plate = do
  onBoard <- ridersOnBoard plate
  results <- forM onBoard $ \b -> (b,) <$> withTryCatch "sharedCab:dropOnFinish" (Seats.markDropped b)
  forM_ (Session.stranded results) $ \b -> logError $ "sharedCab: rider still on board after the session ended, booking=" <> b.id.getId <> " plate=" <> plate

-- | What rider-app saw, told to driver-app (which owns the run); its answer, if the run changed, is applied to the copy.
cabEvent :: CabSyncFlow m r => Session -> CabEvent -> m (Maybe Session)
cabEvent s event = do
  merchant <- CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId)
  resp <- CallBPPInternal.sharedCabCabEvent merchant.driverOfferApiKey merchant.driverOfferBaseUrl s.vehicleNumber event
  traverse applyCabUpdate resp.update

-- | A cab whose last rider got off takes the route queued for after the last drop; True when it switched.
cabEmptied :: CabSyncFlow m r => Text -> m Bool
cabEmptied plate =
  Session.readSession plate >>= \case
    Just s | isJust s.queuedRouteCode,
             s.status /= ENDED -> do
      onBoard <- ridersOnBoard plate
      if null onBoard
        then
          withTryCatch "sharedCab:cabEmptied" (cabEvent s CAB_EMPTIED) >>= \case
            Left err -> False <$ logError ("sharedCab: queued route not applied for " <> plate <> ": " <> show err)
            Right switched -> pure (maybe False (\s' -> s'.routeCode /= s.routeCode) switched)
        else pure False
    _ -> pure False

cabRiderOf :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DFTB.FRFSTicketBooking -> m CabRider
cabRiderOf booking = do
  mbRider <- QPerson.findById booking.riderId
  pure
    SCS.CabRider
      { SCS.bookingId = booking.id.getId,
        SCS.firstName = fromMaybe "" (mbRider >>= (.firstName)),
        SCS.dropStop = fromMaybe booking.toStationCode booking.toStationName
      }
