module Beckn.Utils.Time where

import Beckn.Types.Time
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import EulerHS.Prelude

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
