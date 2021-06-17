module Beckn.Utils.SlidingWindowLimiter where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common as Common
import Beckn.Types.Error
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Common
import Data.Time hiding (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra

checkSlidingWindowLimit :: HasField "registrationHitsOpt" r RegistrationHitsOptions => Text -> FlowR r ()
checkSlidingWindowLimit key = do
  hitsLimit <- asks (.registrationHitsOpt.limit)
  hitsLimitResetTime <- asks (.registrationHitsOpt.limitResetTime)
  unlessM (slidingWindowLimiter key hitsLimit hitsLimitResetTime) $
    throwError $ HitsLimitError hitsLimitResetTime

-- Sliding window rate limiter.
-- Returns True if limit is not exceed and further
-- actions should be allowed. False otherwise.

slidingWindowLimiter :: Text -> Int -> Int -> FlowR r Bool
slidingWindowLimiter key frameHitsLim frameLen = do
  currTime <- getCurrentTime
  hits <- fromMaybe [] <$> Redis.getKeyRedis key
  let (filtHits, ret) = slidingWindowLimiterPure currTime hits frameHitsLim frameLen
  when ret $ Redis.setExRedis key filtHits frameLen
  return ret

slidingWindowLimiterPure :: UTCTime -> [Integer] -> Int -> Int -> ([Integer], Bool)
slidingWindowLimiterPure currTime hits frameHitsLim frameLen = do
  -- How it works:
  -- We convert UTCTime value to Integer and `div` it by frameLen to
  -- get its frame number. After that we getting list with
  -- timeFrames from redis and getting number of calls within
  -- current and previous frame. Getting prevFrameWeight
  -- (timePassedSinceCurrFrameStart/frameLen == 1 >= n >= 0) and
  -- doing check (prevFrameHitsLen * prevFrameWeight + currFrameHitsLen < frameHitsLim).
  -- If passed - add currFrame to frames list, save it in redis and return True. False otherwise.
  let currFrame = getTimeFrame currTime
      filtHits = filter (hitsFilter currFrame) hits
      prevFrameHitsLen = length $ filter (prevFrameHitsFilter currFrame) filtHits
      prevFrameWeight = 1 - (fromIntegral (getTimeWithinFrame currTime) :: Double) / frameLen'
      currFrameHitsLen = length $ filter (currFrameHitsFilter currFrame) filtHits
      res = floor (fromIntegral prevFrameHitsLen * prevFrameWeight) + currFrameHitsLen < frameHitsLim
  (if res then currFrame : filtHits else filtHits, res)
  where
    frameLen' :: Num a => a
    frameLen' = fromIntegral frameLen
    getTimeFrame time = getTime time `div` frameLen'
    getTimeWithinFrame time = getTime time `mod` frameLen'
    getTime :: UTCTime -> Integer
    getTime = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    hitsFilter currFrame timeFrame = (timeFrame == currFrame - 1) || (timeFrame == currFrame)
    prevFrameHitsFilter currFrame timeFrame = timeFrame == currFrame - 1
    currFrameHitsFilter currFrame timeFrame = timeFrame == currFrame
