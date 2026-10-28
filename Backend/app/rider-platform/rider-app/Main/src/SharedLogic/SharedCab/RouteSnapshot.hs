-- | A route's live cabs for display paths, read once per pod every couple of seconds: every rider polling the route's cab list, a
-- leg's upcoming cabs or a stop's cabs reads the same sessions, LTS fixes and seat bitmaps. Claim, boarding, drop and cancel never
-- read it: they read sessions and seats fresh under their locks, and the seat engine's hold stays the arbiter.
module SharedLogic.SharedCab.RouteSnapshot
  ( SnapshotCab (..),
    routeSnapshot,
    cabBitmapsOf,
    filterCabLegs,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Enabled (cabItineraryShown, sharedCabEnabled)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import qualified SharedLogic.SharedCab.Lts as Lts
import SharedLogic.SharedCab.Plate (findByPlate)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..))

data SnapshotCab = SnapshotCab
  { session :: Session,
    track :: Maybe LT.VehicleTrackingOnRouteResp,
    -- | seat 1 to capacity, as bytes (the cache wants JSON)
    bitmaps :: [Maybe [Word8]]
  }
  deriving (Show, Generic, ToJSON)

snapshotTtlSec :: Seconds
snapshotTtlSec = 2

cabBitmapsOf :: SnapshotCab -> [Maybe ByteString]
cabBitmapsOf c = map (fmap BS.pack) c.bitmaps

-- | The route's ACTIVE sessions, their fixes from one HGETALL of the LTS route hash, and each cab's seat bitmaps from one MGET.
routeSnapshot ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, CacheFlow m r, MonadMask m, CoreMetrics m) =>
  Text ->
  m [SnapshotCab]
routeSnapshot routeCode = IM.withInMemCache ["SharedCabRouteSnapshot", routeCode] snapshotTtlSec $ do
  sessions <- Session.activeSessionsOnRoute routeCode
  positions <- either (const []) identity <$> withTryCatch "sharedCab:snapshot:positions" (Lts.readRoutePositions routeCode)
  forM sessions $ \s -> do
    bitmaps <- Seats.cabBitmaps s
    pure SnapshotCab {session = s, track = findByPlate (.vehicleNumber) s.vehicleNumber positions, bitmaps = map (fmap BS.unpack) bitmaps}

-- | A shared-cab itinerary is shown only while a cab is running its route. TODO: also require the cab not to have passed the board stop.
filterCabLegs :: (Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadMask m, CoreMetrics m) => Id DMOC.MerchantOperatingCity -> [MultiModalTypes.MultiModalLeg] -> m (Maybe [MultiModalTypes.MultiModalLeg])
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
