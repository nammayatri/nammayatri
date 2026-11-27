module SharedLogic.SharedCab.Demand
  ( searchWindowMin,
    recordSearch,
    recordWaitHit,
    markSessionSeen,
    recentMembers,
    waitHitsAtStops,
    StopDemand (..),
    demandByStop,
    waitingRiders,
    demandOnRoutes,
    candidateRiders,
    tallyAcrossStops,
    demandAheadRows,
    searchingRiders,
    findingByRoute,
    tallyDemand,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import Data.Either (lefts)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Database.Redis as RawRedis
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.SharedCabSession as SCS
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Select (groupByKey)
import SharedLogic.SharedCab.Booking (cityLiveWindowSec, findingOnRoute, liveSince, shared)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Interest (interestedOnRoutes)
import SharedLogic.SharedCab.Keys (activeDemandRoutesKey, demandKey, sessionSeenKey, waitHitsKey)
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

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

posixScore :: UTCTime -> Double
posixScore = realToFrac . utcTimeToPOSIXSeconds

expireAsCount :: RawRedis.Redis (Either RawRedis.Reply Bool) -> RawRedis.Redis (Either RawRedis.Reply Integer)
expireAsCount = fmap (fmap (\set -> if set then 1 else 0))

logPipelineFailure :: (MonadFlow m) => Text -> Either SomeException [Either RawRedis.Reply a] -> m ()
logPipelineFailure what = \case
  Left err -> logError (what <> show err)
  Right replies -> whenJust (listToMaybe (lefts replies)) $ \reply -> logError (what <> show reply)

recordWaitHit :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> Text -> Text -> [Text] -> Text -> m Bool
recordWaitHit cfg cityId stopCode routeCodes riderId
  | not cfg.demandNudgeEnabled = pure False
  | otherwise = do
    now <- getCurrentTime
    let ttl = toInteger (cfg.nudgeWindowSec + bucketSec)
    shared $ do
      hitsKey <- Redis.buildKey (waitHitsKey cityId stopCode (bucketOf now))
      routesKey <- Redis.buildKey (activeDemandRoutesKey cityId)
      let routeOps =
            if null routeCodes
              then []
              else [RawRedis.zadd routesKey [(posixScore now, TE.encodeUtf8 r) | r <- nub routeCodes], expireAsCount (RawRedis.expire routesKey ttl)]
      res <- Redis.runPipelined "sharedCabWaitHits" identity ([RawRedis.hincrby hitsKey (TE.encodeUtf8 riderId) 1, expireAsCount (RawRedis.expire hitsKey ttl)] <> routeOps)
      logPipelineFailure "shared-cab wait hit not recorded: " res
      pure $ case res of
        Right (Right 1 : _) -> True
        _ -> False

markSessionSeen :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> Text -> Text -> m ()
markSessionSeen cfg cityId plate = when cfg.demandNudgeEnabled $ do
  now <- getCurrentTime
  let score = posixScore now
  shared $ do
    key <- Redis.buildKey (sessionSeenKey cityId)
    res <-
      Redis.runPipelined
        "sharedCabSessionSeen"
        identity
        [ RawRedis.zadd key [(score, TE.encodeUtf8 plate)],
          RawRedis.zremrangebyscore key 0 (score - fromIntegral cfg.nudgeSessionSeenSec),
          expireAsCount (RawRedis.expire key (toInteger (2 * cfg.nudgeSessionSeenSec)))
        ]
    logPipelineFailure "shared-cab session seen not recorded: " res

recentMembers :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> UTCTime -> m [Text]
recentMembers key windowSec now = shared $ do
  let end = posixScore now
      start = end - fromIntegral windowSec
  prefixed <- Redis.buildKey key
  res <-
    Redis.runPipelined
      "sharedCabRecentMembers"
      identity
      [ fmap (const []) <$> RawRedis.zremrangebyscore prefixed 0 (start - 0.001),
        RawRedis.zrangebyscore prefixed start end
      ]
  logPipelineFailure "shared-cab recent members not read: " res
  pure $ case res of
    Right [_, Right members] -> map TE.decodeUtf8 members
    _ -> []

waitHitsAtStops :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Text -> UTCTime -> [Text] -> m [(Text, Text, Int)]
waitHitsAtStops windowSec cityId now stopCodes = do
  let buckets = max 1 (ceiling (fromIntegral windowSec / fromIntegral bucketSec :: Double))
      keys = [(stopCode, waitHitsKey cityId stopCode (bucketOf now - i)) | stopCode <- stopCodes, i <- [0 .. buckets - 1]]
  res <- shared $ do
    prefixed <- traverse (Redis.buildKey . snd) keys
    Redis.runPipelined "sharedCabWaitHitsRead" RawRedis.hgetall prefixed
  logPipelineFailure "shared-cab wait hits not read: " res
  pure $ case res of
    Right replies ->
      [ (stopCode, TE.decodeUtf8 rider, hits)
        | ((stopCode, _), Right fields) <- zip keys replies,
          (rider, count) <- fields,
          Just hits <- [readMaybe (BS8.unpack count)]
      ]
    Left _ -> []

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

-- | The stops the driver's route cards count, one rider at one stop. A FINDING rider counts at their board stop if it is a candidate of
-- their route; an interested rider has no stop on record and counts at the route's stand. Kept 3 s in memory per city and route list.
demandOnRoutes :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r, CoreMetrics m) => Config.SharedCabTunables -> Text -> [(Text, [Text])] -> m [SCS.StopDemandCount]
demandOnRoutes cfg cityId candidates = IM.withInMemCache ("SharedCabRoutesDemand" : cityId : map fst candidates) 3 $ do
  now <- getCurrentTime
  finding <- findingByRoute . filter (isRecentFinding cfg.findingTimeoutSec now . (.createdAt)) <$> QFRFSTicketBooking.findSharedCabFindingInCity (Id cityId) (liveSince cityLiveWindowSec now)
  interested <- interestedOnRoutes cfg (map fst candidates)
  searched <- searchedAtStops cityId now (nub (concatMap snd candidates))
  pure $ tallyAcrossStops (candidateRiders (Map.map (map (\b -> (b.fromStationCode, b.riderId.getId))) finding) (Map.fromList interested) candidates) searched

-- | (stop, rider) for every waiting rider the candidate stops account for.
candidateRiders :: Map.Map Text [(Text, Text)] -> Map.Map Text [Text] -> [(Text, [Text])] -> [(Text, Text)]
candidateRiders finding interested = concatMap onRoute
  where
    onRoute (routeCode, stops) =
      [entry | entry@(stopCode, _) <- Map.findWithDefault [] routeCode finding, stopCode `elem` stops]
        <> [(stand, rider) | stand <- take 1 stops, rider <- Map.findWithDefault [] routeCode interested]

-- | Each rider counts once across all the stops: waiting wins over searching, and the earlier stop wins over a later one.
tallyAcrossStops :: [(Text, Text)] -> [(Text, Set.Set Text)] -> [SCS.StopDemandCount]
tallyAcrossStops findingRiders searchedByStop =
  [ SCS.StopDemandCount {SCS.stopCode = stopCode, SCS.waiting = Just (Set.size waiting), SCS.searching = Just (Set.size searching)}
    | ((stopCode, waiting), searching) <- zip (zip (map fst searchedByStop) waitingPerStop) searchingPerStop
  ]
  where
    waitingAt stopCode = Set.fromList [rider | (code, rider) <- findingRiders, code == stopCode]
    distinct seen riders = let fresh = riders `Set.difference` seen in (seen <> fresh, fresh)
    (allWaiting, waitingPerStop) = mapAccumL distinct Set.empty (map (waitingAt . fst) searchedByStop)
    (_, searchingPerStop) = mapAccumL distinct allWaiting (map snd searchedByStop)

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
