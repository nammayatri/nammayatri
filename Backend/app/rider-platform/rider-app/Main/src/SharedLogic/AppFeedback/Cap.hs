-- | The rider's feedback cap: at most one feedback card every minGapHours and maxPerWeek cards in any
-- 7 days, across app and ride feedback. The times cards were shown are kept per rider in a Redis sorted
-- set (member = the response, score = epoch seconds) that expires a week after the last one; nextAllowedAt is worked out from
-- them, never stored. Losing the set only lets a card through early, so it is not rebuilt from the DB.
module SharedLogic.AppFeedback.Cap
  ( CapSettings (..),
    capSettings,
    recentShownTimes,
    recordShown,
    nextAllowedAt,
    capLockKey,
  )
where

import Data.List (sort)
import Data.List.NonEmpty (nonEmpty)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

data CapSettings = CapSettings
  { minGapHours :: Int,
    maxPerWeek :: Int
  }

capSettings :: DRC.RiderConfig -> CapSettings
capSettings riderConfig =
  CapSettings
    { minGapHours = fromMaybe 24 riderConfig.appFeedbackMinGapHours,
      maxPerWeek = fromMaybe 3 riderConfig.appFeedbackMaxPerWeek
    }

window :: NominalDiffTime
window = 7 * 86400

shownKey :: Id DP.Person -> Text
shownKey personId = "AppFeedback:Shown:PersonId-" <> personId.getId

-- | Held while a card is checked against the cap and recorded, so two screens or devices cannot both
-- take the last slot.
capLockKey :: Id DP.Person -> Text
capLockKey personId = "AppFeedback:CapLock:PersonId-" <> personId.getId

-- | When the rider was shown a feedback card in the last 7 days, oldest first.
recentShownTimes :: (Hedis.HedisFlow m r, MonadFlow m) => Id DP.Person -> UTCTime -> m [UTCTime]
recentShownTimes personId now = do
  members <- Hedis.zRangeWithScores (shownKey personId) 0 (-1)
  pure $ sort [t | (_, score) <- members, let t = posixSecondsToUTCTime (realToFrac score), t > addUTCTime (negate window) now]

-- | Records a card shown now. The member is the response it was shown as, so reporting the same card
-- as SHOWN again (the app refetched while it was open) moves its time instead of counting it twice.
recordShown :: (Hedis.HedisFlow m r, MonadFlow m) => Id DP.Person -> UTCTime -> Text -> m ()
recordShown personId now responseId = do
  Hedis.zAddExp (shownKey personId) responseId (floor (utcTimeToPOSIXSeconds now)) (8 * 86400)
  void $ Hedis.zRemRangeByScore (shownKey personId) 0 (epoch (addUTCTime (negate window) now))

-- | The earliest time the next card may be shown; Nothing when one may be shown now.
nextAllowedAt :: CapSettings -> UTCTime -> [UTCTime] -> Maybe UTCTime
nextAllowedAt settings now shown =
  let recent = sort [t | t <- shown, t > addUTCTime (negate window) now]
      byGap = addUTCTime (fromIntegral settings.minGapHours * 3600) <$> lastMay recent
      perWeek = max 1 settings.maxPerWeek
      -- A slot frees up when the card that fills the week leaves the 7-day window.
      byWeek
        | length recent >= perWeek = addUTCTime window <$> listToMaybe (drop (length recent - perWeek) recent)
        | otherwise = Nothing
      next = maximum <$> nonEmpty (catMaybes [byGap, byWeek])
   in mfilter (> now) next

epoch :: UTCTime -> Double
epoch = realToFrac . utcTimeToPOSIXSeconds
