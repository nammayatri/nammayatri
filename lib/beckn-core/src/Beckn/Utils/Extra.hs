module Beckn.Utils.Extra where

import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude

getCurrentTimeUTC :: L.Flow LocalTime
getCurrentTimeUTC = L.runIO' "getCurrentTimeUTC" getCurrentTimeUTC'

getCurrentTimeUTC' :: IO LocalTime
getCurrentTimeUTC' = (zonedTimeToLocalTime . utcToZonedTime utc) <$> getCurrentTime

addIfPresent :: [a] -> Maybe a -> [a]
addIfPresent xs (Just x) = x : xs
addIfPresent xs _ = xs

isExpired :: NominalDiffTime -> LocalTime -> L.Flow Bool
isExpired nominal time = do
  now <- getCurrentTimeUTC
  let addedLocalTime = addLocalTime nominal time
  return $ now > addedLocalTime

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x
