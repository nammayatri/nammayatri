module SharedLogic.SharedCab.DemandNudge
  ( NudgeStop (..),
    OnRouteCab (..),
    NudgeRoute (..),
    DemandAhead (..),
    assignRiders,
    suffixAhead,
    aheadAt,
    cabNextIdx,
    runDemandNudge,
  )
where

import Data.List (findIndex, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Route as Fields
import Domain.Types.RouteStopMapping as Fields
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.CallBPPInternal (NudgeRoute (..), NudgeStop (..), OnRouteCab (..))
import SharedLogic.SharedCab.Booking (shared)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Demand as Demand
import SharedLogic.SharedCab.Keys (activeDemandRoutesKey, demandAheadKey, sessionSeenKey)
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest

data DemandAhead = DemandAhead
  { stopCodes :: [Text],
    ahead :: [Int],
    at :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

assignRiders :: Int -> [Text] -> [(Text, Text, Int)] -> Map.Map Text Int
assignRiders minHits stopCodes hits = Map.mapMaybe pick perRider
  where
    index = Map.fromList (zip stopCodes [0 ..])
    perRider = Map.fromListWith (Map.unionWith (+)) [(rider, Map.singleton i n) | (stopCode, rider, n) <- hits, Just i <- [Map.lookup stopCode index]]
    pick byStop
      | sum (Map.elems byStop) >= minHits = Just (snd (maximum [(n, i) | (i, n) <- Map.toList byStop]))
      | otherwise = Nothing

suffixAhead :: Int -> Map.Map Text Int -> [Int]
suffixAhead stopCount assigned = scanr1 (+) [Map.findWithDefault 0 i perStop | i <- [0 .. stopCount - 1]]
  where
    perStop = Map.fromListWith (+) [(i, 1 :: Int) | i <- Map.elems assigned]

aheadAt :: [Int] -> Int -> Int
aheadAt counts i = fromMaybe 0 (listToMaybe (drop i counts))

cabNextIdx :: Config.SharedCabTunables -> UTCTime -> [Text] -> Lts.CabTrack -> Maybe Int
cabNextIdx cfg now stopCodes cab
  | cab.offRoute = Nothing
  | diffUTCTime now cab.pingAt > fromIntegral cfg.ltsMaxAgeSec = Nothing
  | otherwise = Lts.nextStopCode cab >>= \stopCode -> findIndex (== stopCode) stopCodes

type NudgeFlow m r c = (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Metrics.CoreMetrics m, HasShortDurationRetryCfg r c, MonadMask m)

runDemandNudge :: NudgeFlow m r c => Config.SharedCabTunables -> DIBC.IntegratedBPPConfig -> m (Bool, [NudgeRoute], [Text])
runDemandNudge cfg ibc = do
  now <- getCurrentTime
  let cityId = ibc.merchantOperatingCityId.getId
  routeCodes <- Demand.recentMembers (activeDemandRoutesKey cityId) cfg.nudgeWindowSec now
  seen <- Set.fromList <$> Demand.recentMembers (sessionSeenKey cityId) cfg.nudgeSessionSeenSec now
  routes <- forM routeCodes $ \routeCode ->
    withTryCatch "sharedCab:demandNudgeRoute" (routeNudge cfg ibc now seen routeCode) >>= \case
      Left err -> Nothing <$ logError ("shared-cab demand nudge failed for route " <> routeCode <> ": " <> show err)
      Right nudge -> pure nudge
  let nudges = catMaybes routes
  pure (not (null routeCodes), map fst nudges, Set.toList (Set.fromList (concatMap snd nudges)))

routeNudge :: NudgeFlow m r c => Config.SharedCabTunables -> DIBC.IntegratedBPPConfig -> UTCTime -> Set.Set Text -> Text -> m (Maybe (NudgeRoute, [Text]))
routeNudge cfg ibc now seen routeCode = do
  let cityId = ibc.merchantOperatingCityId.getId
  stops <- sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode routeCode ibc
  let stopCodes = map (.stopCode) stops
  hits <- Demand.waitHitsAtStops cfg.nudgeWindowSec cityId now stopCodes
  let ahead = suffixAhead (length stopCodes) (assignRiders cfg.nudgeMinRequestsPerRider stopCodes hits)
  shared $ Redis.setExp (demandAheadKey cityId routeCode) (DemandAhead stopCodes ahead now) (2 * cfg.nudgeIntervalSec)
  if aheadAt ahead 0 < cfg.nudgeMinRiders
    then pure Nothing
    else do
      cabs <- Lts.readRoutePositions ibc.id routeCode
      let candidates =
            [ (cab, OnRouteCab {vehicleNumber = cab.vehicleNumber, nextIdx})
              | cab <- cabs,
                not (Set.member cab.vehicleNumber seen),
                Just nextIdx <- [cabNextIdx cfg now stopCodes cab],
                aheadAt ahead nextIdx >= cfg.nudgeMinRiders
            ]
      onRoute <- map snd <$> filterM (hasSeatsAhead . fst) candidates
      routeName <- maybe routeCode (.shortName) <$> OTPRest.getRouteByRouteId ibc routeCode
      pure $
        Just
          ( NudgeRoute
              { routeCode,
                routeName,
                stops = [NudgeStop {code = s.stopCode, name = s.stopName, point = s.stopPoint} | s <- stops],
                ahead,
                onRoute
              },
            map (.vehicleNumber) cabs
          )
  where
    hasSeatsAhead cab = hasFreeSeat <$> Session.readSession cab.vehicleNumber
    hasFreeSeat = \case
      Just s | s.status == ACTIVE -> s.capacity - s.walkupCount > 0
      _ -> True
