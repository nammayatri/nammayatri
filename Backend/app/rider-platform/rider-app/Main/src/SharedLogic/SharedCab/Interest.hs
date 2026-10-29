{-# LANGUAGE ScopedTypeVariables #-}

-- | Riders who opened the cab view at a stand count as waiting on the routes through it for a while, booked or not. Only the person id and
-- the time are kept; what leaves the rider app is the count.
module SharedLogic.SharedCab.Interest
  ( recordInterest,
    interestedRiders,
    windowStart,
    interestKeyTtlSec,
    bestEffort,
  )
where

import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (shared)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Keys (interestKey)

-- | Entries scored before this have aged out.
windowStart :: UTCTime -> Int -> Double
windowStart now windowSec = realToFrac (utcTimeToPOSIXSeconds now) - fromIntegral windowSec

-- | The key outlives the window, and never less than 15 minutes.
interestKeyTtlSec :: Config.SharedCabTunables -> Int
interestKeyTtlSec cfg = max (15 * 60) cfg.interestWindowSec

-- | The rider is interested in these routes now; opening again moves their entry, it does not add one.
recordInterest :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> [Text] -> Text -> m ()
recordInterest cfg routeCodes personId = do
  now <- getCurrentTime
  forM_ routeCodes $ \routeCode -> shared $ Redis.zAddExp (interestKey routeCode) personId (floor (utcTimeToPOSIXSeconds now)) (interestKeyTtlSec cfg)

-- | Persons interested within the window; older entries are dropped on the way.
interestedRiders :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> Text -> m [Text]
interestedRiders cfg routeCode = do
  now <- getCurrentTime
  let start = windowStart now cfg.interestWindowSec
  shared $ do
    void $ Redis.zRemRangeByScore (interestKey routeCode) 0 (start - 0.001)
    map TE.decodeUtf8 <$> Redis.zRangeByScore (interestKey routeCode) start (realToFrac (utcTimeToPOSIXSeconds now))

-- | A failure here is logged, never raised: the request that asked for it must still be answered.
bestEffort :: MonadCatch m => (Text -> m ()) -> m a -> m (Maybe a)
bestEffort logFailure action = (Just <$> action) `catch` \(e :: SomeException) -> Nothing <$ logFailure (show e)
