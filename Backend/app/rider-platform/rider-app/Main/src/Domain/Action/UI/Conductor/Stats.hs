module Domain.Action.UI.Conductor.Stats where

import Data.List (partition)
import Data.Time.Calendar (Day)
import qualified Data.Time as Time
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Clickhouse.ConductorStats as CHConductor

data StatsResp = StatsResp
  { todayTickets :: Int,
    todayRevenue :: Money,
    weeklyTickets :: Int,
    weeklyRevenue :: Money,
    mtdTickets :: Int,
    mtdRevenue :: Money,
    busNo :: Maybe Text,
    newUsersToday :: Int,
    newUsersWeekly :: Int,
    newUsersMtd :: Int,
    rangeTickets :: Maybe Int,
    rangeRevenue :: Maybe Money
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Agg = Agg
  { tickets :: Int,
    revenue :: Double,
    newUsers :: Int
  }

emptyAgg :: Agg
emptyAgg = Agg 0 0 0

aggStat :: CHConductor.ConductorStats -> Agg -> Agg
aggStat stat acc =
  Agg
    { tickets = acc.tickets + fromMaybe 0 stat.numberTicketsBooked,
      revenue = acc.revenue + fromMaybe 0 stat.totalRevenueInADay,
      newUsers = acc.newUsers + fromMaybe 0 stat.numberOfNewCustomers
    }

statsHandler :: Text -> Maybe Day -> Maybe Day -> FlowHandler StatsResp
statsHandler conductorToken mbStartDate mbEndDate = withFlowHandlerAPI $ do
  now <- liftIO getCurrentTime
  let today = Time.utctDay now
  let (year, month, _) = Time.toGregorian today
  let monthStartDay = Time.fromGregorian year month 1
  let mjd = Time.toModifiedJulianDay today
  let daysFromFriday = fromInteger $ (mjd - 2 + 7) `mod` 7
  let weekStartDay = Time.addDays (negate daysFromFriday) today

  mtdStats <- CHConductor.findConductorStatsBetween conductorToken monthStartDay today
  logDebug $ "mtdStats: " <> show mtdStats

  weeklyStats <- CHConductor.findConductorStatsBetween conductorToken weekStartDay today
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

  let newUsersToday = todayAgg.newUsers
  let newUsersWeekly = weeklyAgg.newUsers
  let newUsersMtd = mtdAgg.newUsers
  let busNo = Nothing

  (rangeTickets, rangeRevenue) <- case (mbStartDate, mbEndDate) of
    (Just startDate, Just endDate) -> do
      rangeStats <- CHConductor.findConductorStatsBetween conductorToken startDate endDate
      let rangeAgg = foldr aggStat emptyAgg rangeStats
      pure (Just rangeAgg.tickets, Just . Money . round $ rangeAgg.revenue)
    _ -> pure (Nothing, Nothing)

  return StatsResp {..}
