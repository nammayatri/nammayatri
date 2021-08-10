module Beckn.Utils.Time
  ( module Beckn.Utils.Time,
    module Beckn.Types.Time,
  )
where

import Beckn.Types.Time
import Beckn.Utils.Logging
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import EulerHS.Prelude
import System.Clock (toNanoSecs)

isExpired :: MonadTime m => NominalDiffTime -> UTCTime -> m Bool
isExpired nominal time = do
  now <- getCurrentTime
  let addedUTCTime = addUTCTime nominal time
  return $ now > addedUTCTime

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: UTCTime -> Text
showTimeIst time =
  T.pack $
    formatTime defaultTimeLocale "%d %b, %I:%M %p" $
      addUTCTime (60 * 330) time

getClockTimeInMs :: MonadClock m => m Milliseconds
getClockTimeInMs = Milliseconds . fromInteger . (`div` 1000000) . toNanoSecs <$> getClockTime

measureDuration :: MonadClock m => m a -> m (a, Milliseconds)
measureDuration f = do
  start <- getClockTimeInMs
  res <- f
  end <- getClockTimeInMs
  return (res, end - start)

measuringDuration :: (Milliseconds -> a -> m ()) -> MeasuringDuration m a
measuringDuration doWithDuration f = do
  (res, dur) <- measureDuration f
  doWithDuration dur res
  return res

measuringDurationInS :: (Seconds -> a -> m ()) -> MeasuringDuration m a
measuringDurationInS doWithDuration =
  measuringDuration $
    doWithDuration . (`div` 1000) . fromIntegral

measuringDurationToLog :: Log m => LogLevel -> Text -> MeasuringDuration m a
measuringDurationToLog logLevel fname = tabs . measuringDuration $ \duration _ ->
  withLogTag "duration"
    . logOutput logLevel
    $ fname <> " took " <> show duration <> " milliseconds"
  where
    -- debugging feature, use only in dev
    -- tabs = (withLogTag "  " .)
    tabs = id

millisecondsToMicroseconds :: Milliseconds -> Microseconds
millisecondsToMicroseconds (Milliseconds mill) = Microseconds $ mill * 1000