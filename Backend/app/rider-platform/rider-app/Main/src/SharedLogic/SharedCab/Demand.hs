module SharedLogic.SharedCab.Demand
  ( searchWindowMin,
    recordSearch,
    sharedCabBoardStops,
    StopDemand (..),
    demandByStop,
    waitingRiders,
    routeCounts,
    routeDemand,
    demandAheadRows,
    searchingRiders,
    findingByRoute,
    tallyDemand,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Database.Redis as RawRedis
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.SharedCabSession as SCS
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Select (groupByKey)
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
  deriving (Show, Eq, Generic, ToJSON)

-- | A FINDING booking still counts as waiting for the city's findingTimeoutSec after it was made.
isRecentFinding :: Int -> UTCTime -> UTCTime -> Bool
isRecentFinding findingTimeoutSec now createdAt = diffUTCTime now createdAt < fromIntegral findingTimeoutSec

-- | Distinct riders waiting on a route: those with a recent FINDING booking, given as (created at, rider), and those who opened a stand on
-- it within the interest window (`interested`), one person counted once whichever way they are known.
waitingRiders :: Int -> UTCTime -> [(UTCTime, Text)] -> [Text] -> Int
waitingRiders findingTimeoutSec now bookings interested =
  Set.size (Set.fromList ([rider | (createdAt, rider) <- bookings, isRecentFinding findingTimeoutSec now createdAt] <> interested))

-- | The driver's pre-start route card as (waiting, searching), each rider counted once: searching leaves out everyone already
-- waiting, whether known by a FINDING booking or by interest (a rider who opened the stand and searched used to count twice).
routeCounts :: Int -> UTCTime -> [(UTCTime, Text)] -> [Text] -> [Set.Set Text] -> (Int, Int)
routeCounts findingTimeoutSec now bookings interested searchedPerStop = (Set.size waiting, searchingRiders searchedPerStop waiting)
  where
    waiting = Set.fromList ([rider | (createdAt, rider) <- bookings, isRecentFinding findingTimeoutSec now createdAt] <> interested)

-- | 'routeCounts' for one route; its FINDING bookings come from 'findingByRoute', read once for the whole route list.
routeDemand :: (CacheFlow m r, MonadFlow m, Redis.HedisFlow m r, MonadCatch m) => Config.SharedCabTunables -> Text -> [DFRFSTicketBooking.FRFSTicketBooking] -> Text -> [Text] -> m (Int, Int)
routeDemand cfg cityId routeFinding routeCode stopCodes = do
  now <- getCurrentTime
  interested <- fromMaybe [] <$> bestEffort (logError . ("shared-cab interest read failed: " <>)) (interestedRiders cfg routeCode)
  searched <- map snd <$> searchedAtStops cityId now stopCodes
  pure (routeCounts cfg.findingTimeoutSec now [(b.createdAt, b.riderId.getId) | b <- routeFinding] interested searched)

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

-- | The city's FINDING bookings by route: one query serves every route of the driver's route list.
findingByRoute :: [DFRFSTicketBooking.FRFSTicketBooking] -> Map.Map Text [DFRFSTicketBooking.FRFSTicketBooking]
findingByRoute = groupByKey (.routeCode)

recentFinding :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Config.SharedCabTunables -> UTCTime -> Text -> Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
recentFinding cfg now cityId routeCode = filter (\b -> isRecentFinding cfg.findingTimeoutSec now b.createdAt) <$> findingOnRoute (Id cityId) routeCode

-- | Per board stop of the route: riders whose booking is FINDING (and not past the city's findingTimeoutSec),
-- and riders who searched in the window without booking.
-- | Kept 3 s in memory per city and route: every driver on the route polls it.
demandByStop :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, CoreMetrics m) => Config.SharedCabTunables -> Text -> Text -> [Text] -> m (Map.Map Text StopDemand)
demandByStop cfg cityId routeCode stopCodes = IM.withInMemCache ["SharedCabDemand", cityId, routeCode] 3 $ do
  now <- getCurrentTime
  finding <- recentFinding cfg now cityId routeCode
  searched <- searchedAtStops cityId now stopCodes
  pure $ tallyDemand [(b.fromStationCode, b.riderId.getId) | b <- finding] searched

-- | Every stop's search buckets in one pipelined round trip; a failed read counts no searches.
searchedAtStops :: (CacheFlow m r, MonadFlow m) => Text -> UTCTime -> [Text] -> m [(Text, Set.Set Text)]
searchedAtStops cityId now stopCodes = do
  let keys = [(stopCode, demandKey cityId stopCode (bucketOf now - i)) | stopCode <- stopCodes, i <- [0 .. bucketCount - 1]]
  replies <- Redis.withMasterRedis $ do
    prefixed <- traverse (Redis.buildKey . snd) keys
    Redis.runPipelined "sharedCabDemandSearches" RawRedis.smembers prefixed
  let members = either (const (map (const []) keys)) (map (either (const []) (mapMaybe A.decodeStrict))) replies
  pure [(stopCode, Set.unions [Set.fromList ms | ((code, _), ms) <- zip keys members, code == stopCode]) | stopCode <- stopCodes]

tallyDemand :: [(Text, Text)] -> [(Text, Set.Set Text)] -> Map.Map Text StopDemand
tallyDemand findingRiders searchedByStop =
  Map.fromList
    [ (stopCode, StopDemand {waiting = Set.size waiting, searching = Set.size (searched `Set.difference` waiting)})
      | (stopCode, searched) <- searchedByStop,
        let waiting = Map.findWithDefault Set.empty stopCode waitingAt
    ]
  where
    waitingAt = Map.fromListWith Set.union [(stopCode, Set.singleton rider) | (stopCode, rider) <- findingRiders]
