module SharedLogic.EditLocationThrottle
  ( gateEditLocAttempts,
    decEditLocSoftAttempts,
    decEditLocConfirmAttempts,
    decEditPickupAttempts,
    clearBookingEditAttempts,
  )
where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingUpdateRequest as DBUR
import Environment (Flow)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

locAttemptsKey :: Id DB.Booking -> Text
locAttemptsKey bookingId = "BAP:BookingEditLocAttempts:" <> bookingId.getId

pickupAttemptsKey :: Id DB.Booking -> Text
pickupAttemptsKey bookingId = "BAP:BookingEditPickupAttempts:" <> bookingId.getId

locConfirmField :: Text
locConfirmField = "location:CONFIRM"

locSoftField :: Text
locSoftField = "location:SOFT"

locSoftLimit :: Int
locSoftLimit = 10

locTtlSeconds :: Int
locTtlSeconds = 3 * 60 * 60

pickupTtlSeconds :: Int
pickupTtlSeconds = 60 * 60

readOrRehydrateLocAttempt :: Id DB.Booking -> Text -> DBUR.BookingUpdateRequestStatus -> Int -> Flow Int
readOrRehydrateLocAttempt bookingId field statusToCount initialBudget = do
  mbVal <- Hedis.hGet (locAttemptsKey bookingId) field
  case mbVal of
    Just v -> pure v
    Nothing -> do
      burs <- B.runInReplica $ QBUR.findAllByBookingId bookingId
      let used = length $ filter (\b -> b.status == statusToCount) burs
          remaining = max 0 (initialBudget - used)
      Hedis.hSetExp (locAttemptsKey bookingId) field remaining locTtlSeconds
      pure remaining

gateEditLocAttempts :: Id DB.Booking -> Int -> Flow ()
gateEditLocAttempts bookingId threshold = do
  remaining <- readOrRehydrateLocAttempt bookingId locConfirmField DBUR.CONFIRM threshold
  when (remaining <= (0 :: Int)) $ throwError EditLocationAttemptsExhausted

decEditLocSoftAttempts :: Id DB.Booking -> Flow ()
decEditLocSoftAttempts bookingId = do
  remaining <- readOrRehydrateLocAttempt bookingId locSoftField DBUR.SOFT locSoftLimit
  when (remaining <= (0 :: Int)) $ throwError EditLocationAttemptsExhausted
  Hedis.hSetExp (locAttemptsKey bookingId) locSoftField (remaining - 1 :: Int) locTtlSeconds

decEditLocConfirmAttempts :: Id DB.Booking -> Int -> Flow ()
decEditLocConfirmAttempts bookingId threshold = do
  remaining <- readOrRehydrateLocAttempt bookingId locConfirmField DBUR.CONFIRM threshold
  Hedis.hSetExp (locAttemptsKey bookingId) locConfirmField (max 0 (remaining - 1 :: Int)) locTtlSeconds

readOrRehydratePickupAttempt :: Id DB.Booking -> Int -> Flow Int
readOrRehydratePickupAttempt bookingId initialBudget = do
  mbVal <- Hedis.get (pickupAttemptsKey bookingId)
  case mbVal of
    Just v -> pure v
    Nothing -> do
      mappings <- B.runInReplica $ QLM.findAllByEntityIdAndOrder bookingId.getId 0
      let used = max 0 (length mappings - 1)
          remaining = max 0 (initialBudget - used)
      Hedis.setExp (pickupAttemptsKey bookingId) remaining pickupTtlSeconds
      pure remaining

decEditPickupAttempts :: Id DB.Booking -> Int -> Flow ()
decEditPickupAttempts bookingId threshold = do
  remaining <- readOrRehydratePickupAttempt bookingId threshold
  when (remaining <= (0 :: Int)) $ throwError EditLocationAttemptsExhausted
  Hedis.setExp (pickupAttemptsKey bookingId) (remaining - 1 :: Int) pickupTtlSeconds

clearBookingEditAttempts :: Id DB.Booking -> Flow ()
clearBookingEditAttempts bookingId = do
  Hedis.del (locAttemptsKey bookingId)
  Hedis.del (pickupAttemptsKey bookingId)
