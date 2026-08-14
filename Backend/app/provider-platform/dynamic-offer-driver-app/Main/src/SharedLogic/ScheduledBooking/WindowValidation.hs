module SharedLogic.ScheduledBooking.WindowValidation
  ( validateScheduledBookingWindow,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error (ScheduledBookingError (..))

-- Defence-in-depth BPP guard: reject a scheduled booking whose lead time
-- (pickup - now) falls outside the configured [min, max] advance window.
-- Each bound is skipped when its config is Nothing, so both-Nothing keeps
-- existing behaviour. Window bounds are primarily enforced by the BAP.
validateScheduledBookingWindow ::
  Maybe Seconds ->
  Maybe Seconds ->
  UTCTime ->
  UTCTime ->
  Either ScheduledBookingError ()
validateScheduledBookingWindow mbMinBookingWindow mbMaxBookingWindow now pickupTime
  | invertedWindow = Left (ScheduledBookingWindowInvalid "Scheduled booking window is misconfigured: minimum advance window exceeds the maximum.")
  | maybe False (leadTime <) mbMinBookingWindow = Left (ScheduledBookingWindowInvalid "Scheduled pickup time is earlier than the minimum advance booking window.")
  | maybe False (leadTime >) mbMaxBookingWindow = Left (ScheduledBookingWindowInvalid "Scheduled pickup time is later than the maximum advance booking window.")
  | otherwise = Right ()
  where
    leadTime = nominalDiffTimeToSeconds (diffUTCTime pickupTime now)
    -- Inverted config (min > max) would reject every booking; flag it distinctly.
    invertedWindow = fromMaybe False ((>) <$> mbMinBookingWindow <*> mbMaxBookingWindow)
