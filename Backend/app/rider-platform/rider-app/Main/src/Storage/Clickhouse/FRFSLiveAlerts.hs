module Storage.Clickhouse.FRFSLiveAlerts where

import qualified Data.Map.Strict as Map
import Data.Time
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Id
import qualified Storage.Clickhouse.FRFSTicketBooking as TB

truncateToHour :: UTCTime -> UTCTime
truncateToHour (UTCTime day diffTime) =
  let seconds = floor diffTime :: Int
      hours = seconds `div` 3600
      truncatedSeconds = hours * 3600
   in UTCTime day (fromIntegral truncatedSeconds)

truncateToMinute :: UTCTime -> UTCTime
truncateToMinute (UTCTime day diffTime) =
  let seconds = floor diffTime :: Int
      minutes = seconds `div` 60
      truncatedSeconds = minutes * 60
   in UTCTime day (fromIntegral truncatedSeconds)

getHourlyBookingCounts ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  [Text] ->
  UTCTime ->
  UTCTime ->
  m [(UTCTime, Text, Int)]
getHourlyBookingCounts merchantOpCityId modes fromTime toTime = do
  results <-
    CH.findAll $
      CH.select_
        (\booking -> CH.notGrouped (booking.createdAt, booking.vehicleType))
        $ CH.filter_
          ( \booking ->
              booking.createdAt >=. fromTime
                CH.&&. booking.createdAt <. toTime
                CH.&&. booking.merchantOperatingCityId CH.==. merchantOpCityId
                CH.&&. modeFilter booking
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE TB.fRFSTicketBookingTTable)

  let grouped =
        Map.fromListWith
          (+)
          [ ((truncateToHour time, vType), 1)
            | (time, vType) <- results
          ]

  pure $
    [ (time, vType, count)
      | ((time, vType), count) <- Map.toList grouped
    ]
  where
    effectiveModes =
      if null modes then ["BUS", "METRO", "SUBWAY"] else modes

    modeFilter booking =
      let base = booking.vehicleType CH.==. head effectiveModes
          rest = tail effectiveModes
       in foldr
            (\v acc -> booking.vehicleType CH.==. v CH.||. acc)
            base
            rest

getCurrentHourCounts ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  [Text] ->
  UTCTime ->
  m [(Text, Int)]
getCurrentHourCounts merchantOpCityId modes fromTime = do
  results <-
    CH.findAll $
      CH.select_
        ( \booking ->
            CH.groupBy booking.vehicleType $ \vType ->
              (vType, CH.count_ booking.bookingId)
        )
        $ CH.filter_
          ( \booking ->
              booking.createdAt >=. fromTime
                CH.&&. booking.merchantOperatingCityId CH.==. merchantOpCityId
                CH.&&. modeFilter booking
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE TB.fRFSTicketBookingTTable)
  pure results
  where
    effectiveModes =
      if null modes then ["BUS", "METRO", "SUBWAY"] else modes

    modeFilter booking =
      let base = booking.vehicleType CH.==. head effectiveModes
          rest = tail effectiveModes
       in foldr
            (\v acc -> booking.vehicleType CH.==. v CH.||. acc)
            base
            rest

getHistoricalCountsForRanges ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  [(UTCTime, UTCTime)] ->
  m [(Text, Int)]
getHistoricalCountsForRanges merchantOpCityId ranges = do
  allResults <- forM ranges $ \(start, end) ->
    CH.findAll $
      CH.select_
        ( \booking ->
            CH.groupBy booking.vehicleType $ \vType ->
              (vType, CH.count_ booking.bookingId)
        )
        $ CH.filter_
          ( \booking ->
              booking.createdAt >=. start
                CH.&&. booking.createdAt <. end
                CH.&&. booking.merchantOperatingCityId CH.==. merchantOpCityId
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE TB.fRFSTicketBookingTTable)

  let grouped = Map.fromListWith (+) (concat allResults)
  pure $ Map.toList grouped

data MinuteCount = MinuteCount
  { mcTime :: UTCTime,
    mcVehicleType :: Text,
    mcCount :: Int
  }
  deriving (Show)

getMinuteCounts ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m [MinuteCount]
getMinuteCounts merchantOpCityId fromTime toTime = do
  results <-
    CH.findAll $
      CH.select_
        (\booking -> CH.notGrouped (booking.createdAt, booking.vehicleType))
        $ CH.filter_
          ( \booking ->
              booking.createdAt >=. fromTime
                CH.&&. booking.createdAt <. toTime
                CH.&&. booking.merchantOperatingCityId CH.==. merchantOpCityId
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE TB.fRFSTicketBookingTTable)

  let grouped =
        Map.fromListWith
          (+)
          [ ((truncateToMinute time, vType), 1)
            | (time, vType) <- results
          ]

  pure $
    [ MinuteCount
        { mcTime = time,
          mcVehicleType = vType,
          mcCount = count
        }
      | ((time, vType), count) <- Map.toList grouped
    ]
