module Domain.Action.UI.Depot.Stats where

import Data.List (partition)
import qualified Data.Set as Set
import qualified Data.Time as Time
import Domain.Action.UI.Conductor.Stats (aggStat, emptyAgg)
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Clickhouse.ConductorStats as CHConductor

data DepotStatsReq = DepotStatsReq
  { depotName :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DepotStatsResp = DepotStatsResp
  { todayTickets :: Int,
    todayRevenue :: Money,
    weeklyTickets :: Int,
    weeklyRevenue :: Money,
    mtdTickets :: Int,
    mtdRevenue :: Money,
    conductorTokens :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

depotStatsHandler :: DepotStatsReq -> FlowHandler DepotStatsResp
depotStatsHandler req = withFlowHandlerAPI $ do
  now <- liftIO getCurrentTime
  let today = Time.utctDay now
  let (year, month, _) = Time.toGregorian today
  let monthStartDay = Time.fromGregorian year month 1
  let mjd = Time.toModifiedJulianDay today
  let daysFromFriday = fromInteger $ (mjd - 2 + 7) `mod` 7
  let weekStartDay = Time.addDays (negate daysFromFriday) today

  mtdStats <- CHConductor.findDepotStatsBetween req.depotName monthStartDay today
  logDebug $ "mtdStats: " <> show mtdStats

  weeklyStats <- CHConductor.findDepotStatsBetween req.depotName weekStartDay today
  logDebug $ "weeklyStats: " <> show weeklyStats

  let (todayStats, _) =
        partition (\stat -> stat.bookingDate == today) mtdStats

  let todayAgg = foldr aggStat emptyAgg todayStats
  let weeklyAgg = foldr aggStat emptyAgg weeklyStats
  let mtdAgg = foldr aggStat emptyAgg mtdStats

  let todayTickets = todayAgg.tickets
  let weeklyTickets = weeklyAgg.tickets
  let mtdTickets = mtdAgg.tickets

  let todayRevenue = Money . round $ todayAgg.revenue
  let weeklyRevenue = Money . round $ weeklyAgg.revenue
  let mtdRevenue = Money . round $ mtdAgg.revenue
  let conductorTokens = Set.toList $ foldr (\stat acc -> Set.insert stat.conductorTokenNo acc) Set.empty mtdStats

  return DepotStatsResp {..}
