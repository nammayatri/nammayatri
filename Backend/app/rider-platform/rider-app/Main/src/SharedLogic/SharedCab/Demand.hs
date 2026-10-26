module SharedLogic.SharedCab.Demand
  ( searchWindowMin,
    recordSearch,
    sharedCabBoardStops,
    StopDemand (..),
    demandByStop,
    waitingRiders,
    waitingOnRoute,
    demandAheadRows,
    searchingRiders,
    searchingOnRoute,
    tallyDemand,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.SharedCabSession as SCS
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (findingOnRoute)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Interest (bestEffort, interestedRiders)
import SharedLogic.SharedCab.Keys (demandKey)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)

bucketSec, bucketCount, bucketTtlSec :: Int
bucketSec = 300
bucketCount = 3
bucketTtlSec = 20 * 60

searchWindowMin :: Int
searchWindowMin = bucketSec * bucketCount `div` 60

bucketOf :: UTCTime -> Int
bucketOf now = floor (utcTimeToPOSIXSeconds now) `div` bucketSec

-- | A rider was shown a shared cab boarding at `stopCode`. Sets of rider ids, not events: the read subtracts the riders already waiting.
recordSearch :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Text -> Text -> m ()
recordSearch cityId riderId stopCode = do
  now <- getCurrentTime
  Redis.withMasterRedis $ Redis.sAddExp (demandKey cityId stopCode (bucketOf now)) [riderId] bucketTtlSec

sharedCabBoardStops :: [MultiModalTypes.MultiModalRoute] -> [Text]
sharedCabBoardStops routes =
  Set.toList . Set.fromList $
    [ stopCode
      | route <- routes,
        leg <- route.legs,
        maybe False isSharedCabAgency (leg.agency >>= (.gtfsId)),
        Just stopCode <- [leg.fromStopDetails >>= (.stopCode)]
    ]

data StopDemand = StopDemand {waiting :: Int, searching :: Int}
  deriving (Show, Eq)

-- | A FINDING booking still counts as waiting for the city's findingTimeoutSec after it was made.
isRecentFinding :: Int -> UTCTime -> UTCTime -> Bool
isRecentFinding findingTimeoutSec now createdAt = diffUTCTime now createdAt < fromIntegral findingTimeoutSec

-- | Distinct riders waiting on a route: those with a recent FINDING booking, given as (created at, rider), and those who opened a stand on
-- it within the interest window (`interested`), one person counted once whichever way they are known.
waitingRiders :: Int -> UTCTime -> [(UTCTime, Text)] -> [Text] -> Int
waitingRiders findingTimeoutSec now bookings interested =
  Set.size (Set.fromList ([rider | (createdAt, rider) <- bookings, isRecentFinding findingTimeoutSec now createdAt] <> interested))

-- | Riders waiting on the route for a cab: the driver's pre-start route cards.
waitingOnRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r, MonadCatch m) => Config.SharedCabTunables -> Text -> m Int
waitingOnRoute cfg routeCode = do
  now <- getCurrentTime
  finding <- map (\(b :: DFRFSTicketBooking.FRFSTicketBooking) -> (b.createdAt, b.riderId.getId)) <$> findingOnRoute routeCode
  interested <- fromMaybe [] <$> bestEffort (logError . ("shared-cab interest read failed: " <>)) (interestedRiders cfg routeCode)
  pure (waitingRiders cfg.findingTimeoutSec now finding interested)

-- | The stops of the route that have anyone waiting or searching, in route order: the driver session's demandAhead.
demandAheadRows :: [(Text, Text)] -> Map.Map Text StopDemand -> [SCS.DemandAtStop]
demandAheadRows stops demand =
  [ SCS.DemandAtStop {SCS.stopCode = code, SCS.stopName = name, SCS.waiting = d.waiting, SCS.searching = d.searching, SCS.windowMin = searchWindowMin}
    | (code, name) <- stops,
      Just d <- [Map.lookup code demand],
      d.waiting + d.searching > 0
  ]

-- | Distinct riders who searched at any of the route's stops within the window and have no recent FINDING booking on it.
searchingRiders :: [Set.Set Text] -> Set.Set Text -> Int
searchingRiders searchedPerStop waiting = Set.size (Set.unions searchedPerStop `Set.difference` waiting)

searchingOnRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Config.SharedCabTunables -> Text -> Text -> [Text] -> m Int
searchingOnRoute cfg cityId routeCode stopCodes = do
  now <- getCurrentTime
  finding <- recentFinding cfg now routeCode
  searched <- mapM (searchedAt cityId now) stopCodes
  pure (searchingRiders searched (Set.fromList [b.riderId.getId | b <- finding]))

recentFinding :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Config.SharedCabTunables -> UTCTime -> Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
recentFinding cfg now routeCode = filter (\b -> isRecentFinding cfg.findingTimeoutSec now b.createdAt) <$> findingOnRoute routeCode

-- | Riders who searched at the stop in the last bucketCount buckets.
searchedAt :: (CacheFlow m r, MonadFlow m) => Text -> UTCTime -> Text -> m (Set.Set Text)
searchedAt cityId now stopCode =
  Set.unions <$> mapM (fmap Set.fromList . Redis.withMasterRedis . Redis.sMembers . demandKey cityId stopCode) [bucketOf now - i | i <- [0 .. bucketCount - 1]]

-- | Per board stop of the route: riders whose booking is FINDING (and not past the city's findingTimeoutSec),
-- and riders who searched in the window without booking.
demandByStop :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Config.SharedCabTunables -> Text -> Text -> [Text] -> m (Map.Map Text StopDemand)
demandByStop cfg cityId routeCode stopCodes = do
  now <- getCurrentTime
  finding <- recentFinding cfg now routeCode
  searched <- forM stopCodes $ \stopCode -> (stopCode,) <$> searchedAt cityId now stopCode
  pure $ tallyDemand [(b.fromStationCode, b.riderId.getId) | b <- finding] searched

tallyDemand :: [(Text, Text)] -> [(Text, Set.Set Text)] -> Map.Map Text StopDemand
tallyDemand findingRiders searchedByStop =
  Map.fromList
    [ (stopCode, StopDemand {waiting = Set.size waiting, searching = Set.size (searched `Set.difference` waiting)})
      | (stopCode, searched) <- searchedByStop,
        let waiting = Map.findWithDefault Set.empty stopCode waitingAt
    ]
  where
    waitingAt = Map.fromListWith Set.union [(stopCode, Set.singleton rider) | (stopCode, rider) <- findingRiders]
