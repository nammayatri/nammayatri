-- | The driver's "are you still on the route?" before an off-route pause: at most one per plate, written under the plate lock.
-- The tick opens it `checkInLeadSec` before the pause and clears it at the pause or when the cab is back on route.
module SharedLogic.SharedCab.CheckIn
  ( CheckIn (..),
    openIfNone,
    readCheckIn,
    answer,
    answerCheckIn,
    clear,
    checkInView,
  )
where

import Domain.Types.SharedCabSession (SessionCheckIn (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import SharedLogic.SharedCab.Keys (checkInKey)
import SharedLogic.SharedCab.Lock (shared)
import SharedLogic.SharedCab.Session (readSession, withPlateLock)
import SharedLogic.SharedCab.SessionState (Session (..), ownedSession)
import Tools.Error (SharedCabSessionError (..))

data CheckIn = CheckIn
  { checkInId :: Text,
    -- | When the cab went off route, for the answer's log
    offSince :: UTCTime,
    deadline :: UTCTime,
    answered :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A minute past the deadline, so a crashed tick cannot leave one behind.
write :: (Redis.HedisFlow m r, MonadFlow m) => Text -> UTCTime -> CheckIn -> m ()
write plate now c = shared $ Redis.setExp (checkInKey plate) c (max 0 (ceiling (diffUTCTime c.deadline now)) + 60)

readCheckIn :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe CheckIn)
readCheckIn = shared . Redis.safeGet . checkInKey

-- | The new check-in, or Nothing when one is already open. Asked every tick of the lead, so an open one is seen without the lock.
openIfNone :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> UTCTime -> UTCTime -> m (Maybe CheckIn)
openIfNone plate offSince deadline =
  readCheckIn plate >>= \case
    Just _ -> pure Nothing
    Nothing ->
      withPlateLock plate $
        readCheckIn plate >>= \case
          Just _ -> pure Nothing
          Nothing -> do
            checkInId <- generateGUIDText
            now <- getCurrentTime
            let c = CheckIn {checkInId, offSince, deadline, answered = False}
            Just c <$ write plate now c

-- | A STILL_ON_ROUTE answers only the open check-in it names; answering it again changes nothing.
answerCheckIn :: Maybe Text -> Maybe CheckIn -> Either SharedCabSessionError CheckIn
answerCheckIn mbId = \case
  Just c | Just c.checkInId == mbId -> Right c {answered = True}
  _ -> Left CheckInClosed

-- | Only the driver of the plate's live session may answer. Every Yes is logged: nothing else bounds how long a driver who keeps
-- answering keeps the riders.
answer :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Maybe Text -> m Session
answer driver plate mbId = withPlateLock plate $ do
  s <- readSession plate >>= either throwError pure . ownedSession driver
  c <- readCheckIn plate >>= either throwError pure . answerCheckIn mbId
  now <- getCurrentTime
  write plate now c
  logInfo $
    "shared-cab check-in: " <> plate <> " still on route " <> s.routeCode <> ", off route "
      <> show (floor (diffUTCTime now c.offSince / 60) :: Int)
      <> " min (check-in "
      <> c.checkInId
      <> ")"
  pure s

-- | Takes the plate's check-in away and hands back what was there. Only the tick opens one, so none read means none to take.
clear :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> m (Maybe CheckIn)
clear plate =
  readCheckIn plate >>= \case
    Nothing -> pure Nothing
    Just _ -> withPlateLock plate $ readCheckIn plate <* shared (Redis.del (checkInKey plate))

-- | The driver sees a check-in only until it is answered.
checkInView :: UTCTime -> CheckIn -> Maybe SessionCheckIn
checkInView now c
  | c.answered = Nothing
  | otherwise = Just SessionCheckIn {checkInId = c.checkInId, deadlineSec = max 0 (floor (diffUTCTime c.deadline now))}
