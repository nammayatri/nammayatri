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
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

type ThrottleFlow m env r = (HedisFlow m env, EsqDBFlow m r, MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r)

editLocAttemptsKey, editPickupAttemptsKey :: Id DB.Booking -> Text
editLocAttemptsKey b = "BAP:BookingEditLocAttempts:" <> b.getId
editPickupAttemptsKey b = "BAP:BookingEditPickupAttempts:" <> b.getId

locConfirmField, locSoftField :: Text
locConfirmField = "location:CONFIRM"
locSoftField = "location:SOFT"

locSoftLimit, locTtl, pickupTtl :: Int
locSoftLimit = 10
locTtl = 3 * 60 * 60
pickupTtl = 60 * 60

readLocField ::
  ThrottleFlow m env r =>
  Id DB.Booking ->
  Text ->
  (DBUR.BookingUpdateRequest -> Bool) ->
  Int ->
  m Int
readLocField bookingId field consumedBy initialBudget = do
  mbVal <- Hedis.hGet (editLocAttemptsKey bookingId) field
  case mbVal of
    Just v -> pure v
    Nothing -> do
      burs <- B.runInReplica $ QBUR.findAllByBookingId bookingId
      let r = max 0 (initialBudget - length (filter consumedBy burs))
      Hedis.hSetExp (editLocAttemptsKey bookingId) field r locTtl
      pure r

gateEditLocAttempts :: ThrottleFlow m env r => Id DB.Booking -> Int -> m ()
gateEditLocAttempts bookingId threshold = do
  remaining <- readLocField bookingId locConfirmField (\b -> b.status == DBUR.CONFIRM) threshold
  when (remaining <= 0) $ throwError EditLocationAttemptsExhausted

decEditLocSoftAttempts :: ThrottleFlow m env r => Id DB.Booking -> m ()
decEditLocSoftAttempts bookingId = do
  remaining <- readLocField bookingId locSoftField (const True) locSoftLimit
  when (remaining <= 0) $ throwError EditLocationAttemptsExhausted
  Hedis.hSetExp (editLocAttemptsKey bookingId) locSoftField (remaining - 1) locTtl

decEditLocConfirmAttempts :: ThrottleFlow m env r => Id DB.Booking -> Int -> m ()
decEditLocConfirmAttempts bookingId threshold = do
  remaining <- readLocField bookingId locConfirmField (\b -> b.status == DBUR.CONFIRM) threshold
  Hedis.hSetExp (editLocAttemptsKey bookingId) locConfirmField (max 0 (remaining - 1)) locTtl

decEditPickupAttempts :: ThrottleFlow m env r => Id DB.Booking -> Int -> m ()
decEditPickupAttempts bookingId threshold = do
  mbVal <- Hedis.get (editPickupAttemptsKey bookingId)
  remaining <- case mbVal of
    Just v -> pure v
    Nothing -> do
      mappings <- B.runInReplica $ QLM.findAllByEntityIdAndOrder bookingId.getId 0
      let r = max 0 (threshold - max 0 (length mappings - 1))
      Hedis.setExp (editPickupAttemptsKey bookingId) r pickupTtl
      pure r
  when (remaining <= 0) $ throwError EditLocationAttemptsExhausted
  Hedis.setExp (editPickupAttemptsKey bookingId) (remaining - 1) pickupTtl

clearBookingEditAttempts :: HedisFlow m env => Id DB.Booking -> m ()
clearBookingEditAttempts bookingId = do
  Hedis.del (editLocAttemptsKey bookingId)
  Hedis.del (editPickupAttemptsKey bookingId)
