-- | A route's live cabs for display paths, read once per pod every couple of seconds: every rider polling the route's cab list, a
-- leg's upcoming cabs or a stop's cabs reads the same sessions, LTS fixes and seat bitmaps. Claim, boarding, drop and cancel never
-- read it: they read sessions and seats fresh under their locks, and the seat engine's hold stays the arbiter.
-- One request rebuilds an expired snapshot while the others keep serving the old one (a plain TTL cache let every queued request
-- rebuild at once under load).
module SharedLogic.SharedCab.RouteSnapshot
  ( SnapshotCab (..),
    routeSnapshot,
    cabBitmapsOf,
    filterCabLegs,
    SnapshotRead (..),
    snapshotRead,
  )
where

import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Enabled (cabItineraryShown, sharedCabEnabled)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..))
import System.IO.Unsafe (unsafePerformIO)

data SnapshotCab = SnapshotCab
  { session :: Session,
    track :: Maybe LT.VehicleTrackingOnRouteResp,
    -- | seat 1 to capacity
    bitmaps :: [Maybe ByteString]
  }

cabBitmapsOf :: SnapshotCab -> [Maybe ByteString]
cabBitmapsOf = (.bitmaps)

data Slot = Slot
  { built :: Maybe ([SnapshotCab], UTCTime),
    rebuilding :: Bool
  }

-- | Per pod, per route. The kernel's in-memory cache has no in-flight marker, so this is a module-level IORef, as KMRL's manager cache.
routeSnapshots :: IORef (Map.Map Text Slot)
routeSnapshots = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE routeSnapshots #-}

data SnapshotRead = Serve | Rebuild | ServeStaleNoRebuild
  deriving (Show, Eq)

-- | Fresh (under 2 s) is served; stale is rebuilt by the first request while the others serve it; past 10 s, or with nothing built
-- yet, the request rebuilds inline.
snapshotRead :: UTCTime -> Maybe UTCTime -> Bool -> SnapshotRead
snapshotRead now mbBuiltAt rebuilding = case mbBuiltAt of
  Nothing -> Rebuild
  Just builtAt
    | age < 2 -> Serve
    | age >= 10 -> Rebuild
    | rebuilding -> ServeStaleNoRebuild
    | otherwise -> Rebuild
    where
      age = diffUTCTime now builtAt

-- | The route's ACTIVE sessions, their fixes from one HGETALL of the LTS route hash, and each cab's seat bitmaps from one MGET.
routeSnapshot ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadMask m) =>
  Text ->
  m [SnapshotCab]
routeSnapshot routeCode = do
  now <- getCurrentTime
  -- decide and, for a rebuild, claim it in one atomic step
  (decision, current) <- liftIO . atomicModifyIORef' routeSnapshots $ \slots ->
    let slot = Map.findWithDefault (Slot Nothing False) routeCode slots
        decision = snapshotRead now (snd <$> slot.built) slot.rebuilding
        slots' = if decision == Rebuild then Map.insert routeCode slot {rebuilding = True} slots else slots
     in (slots', (decision, maybe [] fst slot.built))
  case decision of
    Rebuild -> rebuild `finally` liftIO (atomicModifyIORef' routeSnapshots (\slots -> (Map.adjust (\slot -> slot {rebuilding = False}) routeCode slots, ())))
    _ -> pure current
  where
    rebuild = do
      cabs <- buildSnapshot routeCode
      builtAt <- getCurrentTime
      liftIO . atomicModifyIORef' routeSnapshots $ \slots -> (Map.insert routeCode (Slot (Just (cabs, builtAt)) True) slots, ())
      pure cabs

buildSnapshot ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadMask m) =>
  Text ->
  m [SnapshotCab]
buildSnapshot routeCode = do
  sessions <- Session.activeSessionsOnRoute routeCode
  positions <- either (const []) identity <$> withTryCatch "sharedCab:snapshot:positions" (Lts.readRoutePositions routeCode)
  forM sessions $ \s -> do
    bitmaps <- Seats.cabBitmaps s
    pure SnapshotCab {session = s, track = find ((== s.vehicleNumber) . (.vehicleNumber)) positions, bitmaps}

-- | A shared-cab itinerary is shown only while a cab is running its route. TODO: also require the cab not to have passed the board stop.
filterCabLegs :: (Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadMask m) => Id DMOC.MerchantOperatingCity -> [MultiModalTypes.MultiModalLeg] -> m (Maybe [MultiModalTypes.MultiModalLeg])
filterCabLegs cityId legs = case filter isSharedCabLeg legs of
  [] -> pure (Just legs)
  cabLegs -> do
    enabled <- sharedCabEnabled cityId
    cabsComing <- mapM hasCabComing cabLegs
    pure $ if cabItineraryShown enabled cabsComing then Just (filter (not . isSharedCabLeg) legs) else Nothing
  where
    isSharedCabLeg leg = maybe False isSharedCabAgency (leg.agency >>= (.gtfsId))
    hasCabComing leg =
      maybe (return False) (fmap (not . null) . routeSnapshot . gtfsIdtoDomainCode) $
        listToMaybe leg.routeDetails >>= (.gtfsId)
