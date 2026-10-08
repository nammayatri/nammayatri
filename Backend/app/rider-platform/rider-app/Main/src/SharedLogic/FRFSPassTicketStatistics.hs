module SharedLogic.FRFSPassTicketStatistics
  ( TicketUsage (..),
    bookingUsage,
    recordTickets,
    releaseTickets,
    moveTickets,
  )
where

import Data.List (nub, sort)
import qualified Data.Time as T
import qualified Domain.Types.FRFSPassTicketStatistics as DFPTS
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSPassTicketStatistics as QFPTS

data TicketUsage = TicketUsage
  { tickets :: Int,
    fare :: HighPrecMoney,
    saved :: HighPrecMoney
  }
  deriving (Show, Eq)

data Attempt a = Done a | Retry | GiveUp

bookingUsage :: DFRFSTicketBooking.FRFSTicketBooking -> Int -> TicketUsage
bookingUsage booking quantity =
  TicketUsage
    { tickets = max 1 quantity,
      fare = bookingFare,
      saved = max 0 (bookingFare - fromMaybe bookingFare booking.overriddenAmount)
    }
  where
    bookingFare = booking.totalPrice.amount

recordTickets :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> T.Day -> TicketUsage -> m ()
recordTickets payment day usage = adjustTickets payment day usage

releaseTickets :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> T.Day -> TicketUsage -> m ()
releaseTickets payment day usage = adjustTickets payment day (negateUsage usage)

moveTickets :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> T.Day -> T.Day -> TicketUsage -> m ()
moveTickets payment fromDay toDay usage =
  unless (fromDay == toDay) . void . withRetries label . withDayLocks payment [fromDay, toDay] $ do
    removed <- applyDelta payment fromDay (negateUsage usage)
    if isEmptyUsage removed
      then pure (Done ())
      else
        withTryCatch "FRFSPassTicketStatistics:moveIn" (applyDelta payment toDay (negateUsage removed)) >>= \case
          Right _ -> pure (Done ())
          Left err ->
            withTryCatch "FRFSPassTicketStatistics:moveRestore" (applyDelta payment fromDay (negateUsage removed)) >>= \case
              Right _ -> do
                logWarning $ "FRFSPassTicketStatistics: move into the new day failed, restored the old day, retrying " <> label <> " err=" <> show err
                pure Retry
              Left restoreErr -> do
                logError $ "FRFSPassTicketStatistics:DRIFT move failed and the old day could not be restored " <> label <> " moved=" <> show (negateUsage removed) <> " err=" <> show err <> " restoreErr=" <> show restoreErr
                pure GiveUp
  where
    label = "paymentId=" <> payment.id.getId <> " fromDay=" <> show fromDay <> " toDay=" <> show toDay <> " usage=" <> show usage

adjustTickets :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> T.Day -> TicketUsage -> m ()
adjustTickets payment day delta =
  void . withRetries label . withDayLocks payment [day] $
    Done <$> applyDelta payment day delta
  where
    label = "paymentId=" <> payment.id.getId <> " day=" <> show day <> " delta=" <> show delta

applyDelta :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> T.Day -> TicketUsage -> m TicketUsage
applyDelta payment day delta =
  QFPTS.findByPrimaryKey day payment.id >>= \case
    Nothing -> do
      let created = clampUsage delta
      unless (isEmptyUsage created) $ do
        now <- getCurrentTime
        QFPTS.create
          DFPTS.FRFSPassTicketStatistics
            { purchasedPassPaymentId = payment.id,
              date = day,
              personId = payment.personId,
              merchantId = payment.merchantId,
              merchantOperatingCityId = payment.merchantOperatingCityId,
              ticketCount = created.tickets,
              fareAmount = Just created.fare,
              savedAmount = Just created.saved,
              createdAt = now,
              updatedAt = now
            }
      pure created
    Just stats -> do
      let current = TicketUsage {tickets = stats.ticketCount, fare = fromMaybe 0 stats.fareAmount, saved = fromMaybe 0 stats.savedAmount}
          updated = clampUsage (addUsage current delta)
      unless (updated == current) $
        QFPTS.updateUsageByPurchasedPassPaymentIdAndDate updated.tickets (Just updated.fare) (Just updated.saved) payment.id day
      pure (addUsage updated (negateUsage current))

addUsage :: TicketUsage -> TicketUsage -> TicketUsage
addUsage a b = TicketUsage {tickets = a.tickets + b.tickets, fare = a.fare + b.fare, saved = a.saved + b.saved}

negateUsage :: TicketUsage -> TicketUsage
negateUsage u = TicketUsage {tickets = negate u.tickets, fare = negate u.fare, saved = negate u.saved}

clampUsage :: TicketUsage -> TicketUsage
clampUsage u = TicketUsage {tickets = max 0 u.tickets, fare = max 0 u.fare, saved = max 0 u.saved}

isEmptyUsage :: TicketUsage -> Bool
isEmptyUsage u = u.tickets == 0 && u.fare == 0 && u.saved == 0

withDayLocks :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> [T.Day] -> m (Attempt a) -> m (Attempt a)
withDayLocks payment days act =
  acquireAll [] (map lockKey (sort (nub days))) >>= \case
    Nothing -> pure Retry
    Just held ->
      ( withTryCatch "FRFSPassTicketStatistics:withDayLocks" act >>= \case
          Right attempt -> pure attempt
          Left err -> do
            logWarning $ "FRFSPassTicketStatistics: adjustment failed, will retry paymentId=" <> payment.id.getId <> " err=" <> show err
            pure Retry
      )
        `finally` mapM_ Redis.unlockRedis held
  where
    lockKey day = "FRFSPassTicketStatistics:Lock-" <> payment.id.getId <> "-" <> show day

    acquireAll held [] = pure (Just held)
    acquireAll held (key : rest) = do
      acquired <- Redis.tryLockRedis key statisticsLockTtlSec
      if acquired
        then acquireAll (key : held) rest
        else do
          mapM_ Redis.unlockRedis held
          pure Nothing

withRetries :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Attempt a) -> m (Maybe a)
withRetries label act = go 1
  where
    go attempt =
      act >>= \case
        Done result -> pure (Just result)
        GiveUp -> pure Nothing
        Retry
          | attempt >= statisticsMaxAttempts -> do
            logError $ "FRFSPassTicketStatistics:DRIFT gave up after " <> show attempt <> " attempts " <> label
            pure Nothing
          | otherwise -> do
            threadDelay (statisticsRetryDelayMicros * attempt)
            go (attempt + 1)

statisticsLockTtlSec :: Int
statisticsLockTtlSec = 10

statisticsMaxAttempts :: Int
statisticsMaxAttempts = 5

statisticsRetryDelayMicros :: Int
statisticsRetryDelayMicros = 200000
