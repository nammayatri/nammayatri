module Domain.Action.UI.Conductor.Stats where

import qualified Data.Time as Time
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Clickhouse.ConductorStats as CHConductor
import Data.List (partition)

data StatsResp = StatsResp
  { todayTickets :: Int,
    todayRevenue :: Money,
    mtdTickets :: Int,
    mtdRevenue :: Money,
    activeCount :: Int,
    busNo :: Maybe Text,
    newUsersToday :: Int,
    newUsersMtd :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Agg = Agg
  { tickets  :: Int,
    revenue  :: Double,
    newUsers :: Int
  }

emptyAgg :: Agg
emptyAgg = Agg 0 0 0

aggStat :: CHConductor.ConductorStats -> Agg -> Agg
aggStat stat acc =
  Agg
    { tickets  = acc.tickets  + fromMaybe 0 stat.numberTicketsBooked,
      revenue  = acc.revenue  + fromMaybe 0 stat.totalRevenueInADay,
      newUsers = acc.newUsers + fromMaybe 0 stat.numberOfNewCustomers
    }

statsHandler :: Text -> FlowHandler StatsResp
statsHandler conductorToken = withFlowHandlerAPI $ do
  now <- liftIO getCurrentTime
  let today = Time.utctDay now
  let (year, month, _) = Time.toGregorian today
  let monthStartDay = Time.fromGregorian year month 1

  mtdStats <- CHConductor.findConductorStatsBetween conductorToken monthStartDay today
  logDebug $ "mtdStats: " <> show mtdStats

  let (todayStats, _) =
        partition (\stat -> stat.bookingDate == today) mtdStats
  let todayAgg = foldr aggStat emptyAgg todayStats
  let mtdAgg   = foldr aggStat emptyAgg mtdStats

  let todayTickets  = todayAgg.tickets
  let mtdTickets    = mtdAgg.tickets

  let todayRevenue  = Money . round $ todayAgg.revenue
  let mtdRevenue    = Money . round $ mtdAgg.revenue

  let newUsersToday = todayAgg.newUsers
  let newUsersMtd   = mtdAgg.newUsers
  let activeCount = 0
  let busNo = Nothing
  return StatsResp {..}
