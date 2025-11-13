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
    activeCount :: Int, -- Will default to 1 if has today's data
    busNo :: Maybe Text, -- Will need to map from conductor token
    newUsersToday :: Int, -- Placeholder for now
    newUsersMtd :: Int -- Placeholder for now
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

statsHandler :: Text -> FlowHandler StatsResp
statsHandler conductorToken = withFlowHandlerAPI $ do
  now <- liftIO getCurrentTime
  let today = Time.utctDay now
  let yesterday = Time.addDays (-1) today
  let yesterdayStart = Time.UTCTime yesterday 0
  let yesterdayEnd = Time.addUTCTime 86400 yesterdayStart -- 24 hours later
  let (year, month, _) = Time.toGregorian yesterday
  let monthStart = Time.UTCTime (Time.fromGregorian year month 1) 0

  allStats <- CHConductor.findConductorStatsByToken conductorToken
  logDebug $ "allStats: " <> show allStats

  let yesterdayStats =
        filter
          ( \stat ->
              stat.bookingDate >= yesterdayStart && stat.bookingDate < yesterdayEnd
          )
          allStats

  logDebug $ "yesterdayStats: " <> show yesterdayStats

  let mtdStats =
        filter
          ( \stat ->
              stat.bookingDate >= monthStart && stat.bookingDate < yesterdayEnd
          )
          allStats
  logDebug $ "mtdStats: " <> show mtdStats

  let yesterdayTickets = sum $ map (.numberTicketsBooked) yesterdayStats
      mtdTickets = sum $ map (.numberTicketsBooked) mtdStats

      -- Calculate revenue from ClickHouse data
      yesterdayRevenue = Money $ floor $ sum $ map (.totalRevenueInADay) yesterdayStats
      mtdRevenue = Money $ floor $ sum $ map (.totalRevenueInADay) mtdStats

      -- TODO
      activeCount = -1

      busNo = Nothing -- TODO: implement bus number mapping

      -- TODO
      newUsersToday = -1
      newUsersMtd = -1

  return StatsResp {..}
