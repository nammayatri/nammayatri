module SharedLogic.SharedCab.Demand
  ( searchWindowMin,
    recordSearch,
    sharedCabBoardStops,
    StopDemand (..),
    demandByStop,
    tallyDemand,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (findingOnRoute)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)

-- `05` §9: searchers are counted over the last 15 min in 5-min buckets; §8.11 findingTimeoutMin.
bucketSec, bucketCount, bucketTtlSec, findingTimeoutSec :: Int
bucketSec = 300
bucketCount = 3
bucketTtlSec = 20 * 60
findingTimeoutSec = 20 * 60

searchWindowMin :: Int
searchWindowMin = bucketSec * bucketCount `div` 60

bucketOf :: UTCTime -> Int
bucketOf now = floor (utcTimeToPOSIXSeconds now) `div` bucketSec

demandKey :: Text -> Text -> Int -> Text
demandKey cityId stopCode bucket = "sharedcab:demand:" <> cityId <> ":" <> stopCode <> ":" <> show bucket

-- | A rider was shown a shared cab boarding at `stopCode`. Sets of rider ids, not events or HLL:
-- the read subtracts the riders already waiting (§9 "distinct riders").
recordSearch :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Text -> Text -> m ()
recordSearch cityId riderId stopCode = do
  now <- getCurrentTime
  Redis.withMasterRedis $ Redis.sAddExp (demandKey cityId stopCode (bucketOf now)) [riderId] bucketTtlSec

-- | Board stops of the shared-cab legs a search showed.
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

-- | Per board stop of the route: riders whose booking is FINDING (and not past findingTimeoutMin),
-- and riders who searched in the window without booking.
demandByStop :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> Text -> [Text] -> m (Map.Map Text StopDemand)
demandByStop cityId routeCode stopCodes = do
  now <- getCurrentTime
  finding <- filter (\b -> diffUTCTime now b.createdAt < fromIntegral findingTimeoutSec) <$> findingOnRoute routeCode
  let buckets = [bucketOf now - i | i <- [0 .. bucketCount - 1]]
  searched <- forM stopCodes $ \stopCode ->
    (stopCode,) . Set.unions <$> mapM (fmap Set.fromList . Redis.withMasterRedis . Redis.sMembers . demandKey cityId stopCode) buckets
  pure $ tallyDemand [(b.fromStationCode, riderOf b) | b <- finding] searched
  where
    riderOf (b :: DFRFSTicketBooking.FRFSTicketBooking) = b.riderId.getId

-- | (board stop, rider) of FINDING bookings and the riders who searched per stop → counts per stop.
-- A rider who searched and then booked counts as waiting only.
tallyDemand :: [(Text, Text)] -> [(Text, Set.Set Text)] -> Map.Map Text StopDemand
tallyDemand findingRiders searchedByStop =
  Map.fromList
    [ (stopCode, StopDemand {waiting = Set.size waiting, searching = Set.size (searched `Set.difference` waiting)})
      | (stopCode, searched) <- searchedByStop,
        let waiting = Map.findWithDefault Set.empty stopCode waitingAt
    ]
  where
    waitingAt = Map.fromListWith Set.union [(stopCode, Set.singleton rider) | (stopCode, rider) <- findingRiders]
