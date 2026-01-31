module Domain.Action.UI.Conductor.Stats where

import qualified Data.Time as Time
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Clickhouse.ConductorStats as CHConductor

data StatsResp = StatsResp
  { yesterdayTickets :: Int,
    yesterdayRevenue :: Money,
    mtdTickets :: Int,
    mtdRevenue :: Money,
    activeCount :: Int,
    busNo :: Maybe Text,
    newUsersToday :: Int,
    newUsersMtd :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

statsHandler :: Text -> FlowHandler StatsResp
statsHandler conductorToken = withFlowHandlerAPI $ do
  now <- liftIO getCurrentTime
  let today = Time.utctDay now
  let yesterday = Time.addDays (-1) today
  let (year, month, _) = Time.toGregorian yesterday
  let monthStartDay = Time.fromGregorian year month 1

  -- For future proofing when yesterdayStats might be extended to todayStats
  let lowerBound = min monthStartDay yesterday
  allStats <- CHConductor.findConductorStatsBetween conductorToken lowerBound yesterday
  logDebug $ "allStats: " <> show allStats

  let yesterdayStats = filter (\stat -> stat.bookingDate == yesterday) allStats

  logDebug $ "yesterdayStats: " <> show yesterdayStats

  let mtdStats =
        filter
          ( \stat ->
              stat.bookingDate >= monthStartDay
                && stat.bookingDate <= yesterday
          )
          allStats
  logDebug $ "mtdStats: " <> show mtdStats

  let yesterdayTickets = sum $ map (fromMaybe 0 . (.numberTicketsBooked)) yesterdayStats

      mtdTickets = sum $ map (fromMaybe 0 . (.numberTicketsBooked)) mtdStats

      -- Calculate revenue from ClickHouse data
      --
      yesterdayRevenue =
        Money . round $
          sum (map (fromMaybe 0 . (.totalRevenueInADay)) yesterdayStats)

      mtdRevenue =
        Money . round $
          sum (map (fromMaybe 0 . (.totalRevenueInADay)) mtdStats)

      -- TODO
      activeCount = 0
      busNo = Nothing -- TODO: implement bus number mapping
      newUsersToday = sum (map (fromMaybe 0 . (.numberOfNewCustomers)) yesterdayStats)
      newUsersMtd = sum (map (fromMaybe 0 . (.numberOfNewCustomers)) mtdStats)
  return StatsResp {..}
