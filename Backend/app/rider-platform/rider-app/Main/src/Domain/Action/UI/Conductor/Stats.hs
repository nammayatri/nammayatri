module Domain.Action.UI.Conductor.Stats where

import qualified Data.Time as Time
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Clickhouse.ConductorStats as CHConductor

data StatsResp = StatsResp
  { todayTickets :: Int,
    todayRevenue :: Money,
    mtdTickets :: Int,
    mtdRevenue :: Money,
    activeCount :: Int, -- Will default to 1 if has today's data
    busNo :: Maybe Text, -- Will need to map from conductor token
    newUsersToday :: Int, -- Placeholder for now
    newUsersMtd :: Int -- Placeholder for now
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

statsHandler :: Text -> FlowHandler StatsResp
statsHandler conductorToken = withFlowHandlerAPI $ do
  today <- liftIO getCurrentTime
  let todayStart = Time.UTCTime (Time.utctDay today) 0
  let todayEnd = Time.addUTCTime 86400 todayStart -- 24 hours later
  let (year, month, _) = Time.toGregorian (Time.utctDay today)
  let monthStart = Time.UTCTime (Time.fromGregorian year month 1) 0

  allStats <- CHConductor.findConductorStatsByToken conductorToken

  let todayStats =
        filter
          ( \stat ->
              stat.bookingDate >= todayStart && stat.bookingDate < todayEnd
          )
          allStats

  let mtdStats =
        filter
          ( \stat ->
              stat.bookingDate >= monthStart && stat.bookingDate < todayEnd
          )
          allStats

  let todayTickets = sum $ map (.numberTicketsBooked) todayStats
      mtdTickets = sum $ map (.numberTicketsBooked) mtdStats

      -- Calculate revenue from ClickHouse data
      todayRevenue = Money $ floor $ sum $ map (.totalRevenueInADay) todayStats
      mtdRevenue = Money $ floor $ sum $ map (.totalRevenueInADay) mtdStats

      -- TODO
      activeCount = -1

      busNo = Nothing -- TODO: implement bus number mapping

      -- TODO
      newUsersToday = -1
      newUsersMtd = -1

  return StatsResp {..}
